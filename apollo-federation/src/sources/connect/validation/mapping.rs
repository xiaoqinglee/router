mod shape;

use std::collections::HashMap;
use std::iter::once;

use apollo_compiler::collections::IndexMap;
use line_col::LineColLookup;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticSeverity;
use lsp_types::Location;
use lsp_types::Position;
use lsp_types::Range;
use url::Url;

pub use self::shape::Shape;
use crate::sources::connect::json_selection::ArrowMethod;
use crate::sources::connect::json_selection::KnownVariable;
use crate::sources::connect::json_selection::LitExpr;
use crate::sources::connect::json_selection::MethodArgs;
use crate::sources::connect::json_selection::NamedSelection;
use crate::sources::connect::json_selection::PathList;
use crate::sources::connect::json_selection::Ranged;
use crate::sources::connect::json_selection::WithRange;
use crate::sources::connect::JSONSelection;
use crate::sources::connect::Key;
use crate::sources::connect::PathSelection;
use crate::sources::connect::SubSelection;

pub struct Document<'a> {
    pub url: Url,
    pub content: &'a str,
}

/// A diagnostic for a particular File
struct DiagnosticWithUrl {
    pub(self) url: Url,
    pub(self) diagnostic: Diagnostic,
}

impl DiagnosticWithUrl {
    fn new(location: Location, message: String, severity: DiagnosticSeverity) -> Self {
        Self {
            url: location.uri,
            diagnostic: Diagnostic {
                range: location.range,
                severity: Some(severity),
                message,
                ..Default::default()
            },
        }
    }

    fn for_locations(
        locations: impl Iterator<Item = Location>,
        message: String,
        severity: DiagnosticSeverity,
    ) -> Vec<Self> {
        locations
            .map(|location| Self::new(location, message.clone(), severity))
            .collect()
    }
}

fn diagnostics_to_map(diagnostics: Vec<DiagnosticWithUrl>) -> HashMap<Url, Vec<Diagnostic>> {
    let mut map = HashMap::with_capacity(4);
    for diagnostic in diagnostics {
        let vec = map.entry(diagnostic.url).or_insert_with(Vec::new);
        if !vec.contains(&diagnostic.diagnostic) {
            vec.push(diagnostic.diagnostic);
        }
    }
    map
}

pub fn validate(
    input_shape: Shape,
    mapping: Document,
    output_shape: Shape,
    variables: IndexMap<String, Shape>,
) -> (Option<JSONSelection>, HashMap<Url, Vec<Diagnostic>>) {
    let line_col = LineColLookup::new(mapping.content);
    let parsed = match JSONSelection::parse(mapping.content) {
        Ok((_, parsed)) => parsed,
        Err(err) => {
            let diagnostic = Diagnostic::new_simple(Range::default(), err.to_string());
            let diagnostic = DiagnosticWithUrl {
                url: mapping.url,
                diagnostic,
            };
            return (None, diagnostics_to_map(vec![diagnostic]));
        }
    };

    let validator = Validator {
        line_col,
        variables,
        mapping_url: mapping.url,
    };

    let actual_output_shape = match &parsed {
        JSONSelection::Named(sub_selection) => {
            validator.resolve_sub_selection(sub_selection, &input_shape)
        }
        JSONSelection::Path(PathSelection { path }) => validator.resolve_path(path, &input_shape),
    };

    let actual_output_shape = match actual_output_shape {
        Ok(output_shape) => output_shape,
        Err(messages) => return (Some(parsed), diagnostics_to_map(messages)),
    };

    let diagnostics = actual_output_shape.check_compatibility_with(output_shape);
    (Some(parsed), diagnostics_to_map(diagnostics))
}

struct Validator<'a> {
    line_col: LineColLookup<'a>,
    mapping_url: Url,
    variables: IndexMap<String, Shape>,
}

impl Validator<'_> {
    pub(crate) fn location<T>(&self, ranged: &impl Ranged<T>) -> Location {
        let range = ranged
            .range()
            .map(|range| {
                let (line, column) = self.line_col.get(range.start);
                let start = Position {
                    line: (line - 1) as u32,
                    character: (column - 1) as u32,
                };
                let (line, column) = self.line_col.get(range.end);
                let end = Position {
                    line: (line - 1) as u32,
                    character: (column - 1) as u32,
                };
                Range { start, end }
            })
            .unwrap_or_default();

        Location {
            uri: self.mapping_url.clone(),
            range,
        }
    }

    fn resolve_path(
        &self,
        path: &WithRange<PathList>,
        input_shape: &Shape,
    ) -> Result<Shape, Vec<DiagnosticWithUrl>> {
        let shape = match path.as_ref() {
            PathList::Var(variable, tail) => {
                match variable.as_ref() {
                    KnownVariable::This | KnownVariable::Args | KnownVariable::Config => {
                        let Some(var_shape) = self.variables.get(variable.as_str()) else {
                            return Err(vec![DiagnosticWithUrl::new(
                                self.location(variable),
                                format!(
                                    "Variable `{variable}` was not set.",
                                    variable = variable.as_str()
                                ),
                                DiagnosticSeverity::ERROR,
                            )]);
                        };
                        self.resolve_path(tail, var_shape)
                    }
                    KnownVariable::Dollar | KnownVariable::AtSign => {
                        // TODO: this is wrong, sometimes, since @ and $ aren't always the same
                        self.resolve_path(tail, input_shape)
                    }
                }
            }
            PathList::Key(key, tail) => {
                let shape = self.get_key(input_shape, key)?;
                self.resolve_path(tail, &shape)
            }
            PathList::Expr(expr, tail) => {
                let mut shape = self.resolve_literal(input_shape, expr)?;
                shape.replace_locations(vec![self.location(path)]);
                self.resolve_path(tail, &shape)
            }
            PathList::Method(name, args, tail) => {
                let shape = self.resolve_method(input_shape, name, args.clone())?;
                self.resolve_path(tail, &shape)
            }
            PathList::Selection(sub_selection) => {
                let shape = self.resolve_sub_selection(sub_selection, input_shape)?;
                Ok(shape)
            }
            PathList::Empty => Ok(input_shape.clone()),
        }?;
        Ok(shape)
    }

    fn resolve_literal(
        &self,
        input_shape: &Shape,
        lit: &WithRange<LitExpr>,
    ) -> Result<Shape, Vec<DiagnosticWithUrl>> {
        let locations = vec![self.location(lit)];
        match lit.as_ref() {
            LitExpr::String(_) => Ok(Shape::String(locations)),
            LitExpr::Number(_) => Ok(Shape::Number(locations)),
            LitExpr::Bool(_) => Ok(Shape::Bool(locations)),
            LitExpr::Null => Ok(Shape::Nullable(Box::new(Shape::Any(locations)))),
            LitExpr::Object(attributes) => {
                let mut inner =
                    IndexMap::with_capacity_and_hasher(attributes.len(), Default::default());
                for (key, value) in attributes {
                    inner.insert(key.as_string(), self.resolve_literal(input_shape, value)?);
                }
                Ok(Shape::Object {
                    attributes: inner,
                    locations,
                })
            }
            LitExpr::Array(inner) => {
                let mut shape = Shape::Any(Vec::new());
                for expr in inner {
                    shape = shape.merge(self.resolve_literal(input_shape, expr)?);
                }
                Ok(Shape::Array {
                    inner: Box::new(shape),
                    locations,
                })
            }
            LitExpr::Path(PathSelection { path }) => self.resolve_path(path, input_shape),
        }
    }

    fn resolve_method(
        &self,
        input_shape: &Shape,
        name: &WithRange<String>,
        args: Option<MethodArgs>,
    ) -> Result<Shape, Vec<DiagnosticWithUrl>> {
        let Some(method) = ArrowMethod::lookup(name.as_str()) else {
            return Err(vec![DiagnosticWithUrl::new(
                self.location(name),
                format!("Method `{name}` not found.", name = name.as_str()),
                DiagnosticSeverity::ERROR,
            )]);
        };
        let args = args;
        let require_one_arg = |args: Option<MethodArgs>| {
            let Some(args) = args else {
                return Err(vec![DiagnosticWithUrl::new(
                    self.location(name),
                    format!(
                        "`->{method}` requires exactly one argument.",
                        method = name.as_str()
                    ),
                    DiagnosticSeverity::ERROR,
                )]);
            };
            if args.args.len() != 1 {
                return Err(vec![DiagnosticWithUrl::new(
                    self.location(&args),
                    format!(
                        "`->{method}` requires exactly one argument.",
                        method = name.as_str()
                    ),
                    DiagnosticSeverity::ERROR,
                )]);
            }
            Ok(args.args.into_iter().next().unwrap())
        };

        match method {
            ArrowMethod::Echo => {
                let arg = require_one_arg(args)?;
                self.resolve_literal(input_shape, &arg)
            }
            ArrowMethod::Map => {
                let arg = require_one_arg(args)?;
                let mut inner_shape = self.resolve_literal(input_shape, &arg)?;
                if let Shape::Array { .. } = input_shape {
                    let mut locations = inner_shape.locations();
                    locations.push(self.location(name));
                    inner_shape = Shape::Array {
                        inner: Box::new(inner_shape),
                        locations,
                    };
                } else {
                    inner_shape.add_location(self.location(name));
                }
                Ok(inner_shape)
            }
            ArrowMethod::First | ArrowMethod::Last => {
                if let Some(args) = args {
                    return Err(vec![DiagnosticWithUrl::new(
                        self.location(&args),
                        format!("`->{name}` takes no arguments.", name = name.as_str()),
                        DiagnosticSeverity::ERROR,
                    )]);
                }
                match &input_shape {
                    Shape::Array { inner, .. } => {
                        let mut shape = *inner.clone();
                        shape.add_location(self.location(name));
                        Ok(shape)
                    }
                    Shape::Any(_) | Shape::String(_) => Ok(input_shape.clone()),
                    _ => Err(input_shape
                        .locations()
                        .into_iter()
                        .chain(once(self.location(name)))
                        .map(|location| {
                            DiagnosticWithUrl::new(
                                location,
                                format!(
                                    "`->{name}` can only be applied to an array or string.",
                                    name = name.as_str()
                                ),
                                DiagnosticSeverity::ERROR,
                            )
                        })
                        .collect()),
                }
            }
            ArrowMethod::Slice => {
                match &input_shape {
                    Shape::Array { .. } => Ok(input_shape.clone()),
                    Shape::Any(_) | Shape::String(_) => Ok(input_shape.clone()),
                    _ => Err(DiagnosticWithUrl::for_locations(
                        input_shape
                            .locations()
                            .into_iter()
                            .chain(once(self.location(name))),
                        format!(
                            "`->{name}` can only be applied to an array or string.",
                            name = name.as_str()
                        ),
                        DiagnosticSeverity::ERROR,
                    )),
                }
                // TODO: check arguments
            }
            ArrowMethod::Size => {
                let locations = input_shape
                    .locations()
                    .into_iter()
                    .chain(once(self.location(name)));
                match &input_shape {
                    Shape::Array { .. } | Shape::Object { .. } | Shape::Any(_) => {
                        Ok(Shape::Number(locations.collect()))
                    }
                    _ => Err(DiagnosticWithUrl::for_locations(
                        locations,
                        format!(
                            "`->{name}` can only be applied to an array or object.",
                            name = name.as_str()
                        ),
                        DiagnosticSeverity::ERROR,
                    )),
                }
            }
            ArrowMethod::Match => {
                let arg = require_one_arg(args)?;
                let LitExpr::Array(cases) = arg.as_ref() else {
                    return Err(vec![DiagnosticWithUrl::new(
                        self.location(&arg),
                        format!(
                            "`->{name}` requires an array of pairs.",
                            name = name.as_str()
                        ),
                        DiagnosticSeverity::ERROR,
                    )]);
                };
                let mut locations = input_shape.locations();
                locations.push(self.location(name));
                if cases.is_empty() {
                    return Err(DiagnosticWithUrl::for_locations(
                        locations.into_iter(),
                        format!(
                            "`->{name}` requires at least one pair.",
                            name = name.as_str()
                        ),
                        DiagnosticSeverity::ERROR,
                    ));
                }
                let mut shape = Shape::Any(locations);
                for case in cases {
                    let LitExpr::Array(case_pair) = case.as_ref() else {
                        return Err(vec![DiagnosticWithUrl::new(
                            self.location(case),
                            format!(
                                "`->{name}` requires an array of pairs.",
                                name = name.as_str()
                            ),
                            DiagnosticSeverity::ERROR,
                        )]);
                    };
                    if case_pair.len() != 2 {
                        return Err(vec![DiagnosticWithUrl::new(
                            self.location(case),
                            format!(
                                "`->{name}` requires an array of pairs.",
                                name = name.as_str()
                            ),
                            DiagnosticSeverity::ERROR,
                        )]);
                    }
                    shape = shape.merge(self.resolve_literal(input_shape, &case_pair[1])?);
                }
                Ok(shape)
            }
            ArrowMethod::Entries => {
                if let Some(args) = args {
                    return Err(vec![DiagnosticWithUrl::new(
                        self.location(&args),
                        format!("`->{name}` takes no arguments.", name = name.as_str()),
                        DiagnosticSeverity::ERROR,
                    )]);
                }
                match &input_shape {
                    // TODO: better types
                    Shape::Object { locations, .. } | Shape::Any(locations) => Ok(Shape::Array {
                        locations: locations
                            .iter()
                            .cloned()
                            .chain(once(self.location(name)))
                            .collect(),
                        inner: Box::new(Shape::Any(vec![self.location(name)])),
                    }),
                    _ => Err(DiagnosticWithUrl::for_locations(
                        input_shape
                            .locations()
                            .into_iter()
                            .chain(once(self.location(name))),
                        format!(
                            "`->{name}` can only be applied to an object.",
                            name = name.as_str()
                        ),
                        DiagnosticSeverity::ERROR,
                    )),
                }
            }
            #[cfg(test)]
            ArrowMethod::TypeOf => {
                todo!("Implement TypeOf method")
            }
            #[cfg(test)]
            ArrowMethod::Eq => {
                todo!("Implement Eq method")
            }
            #[cfg(test)]
            ArrowMethod::MatchIf => {
                todo!("Implement MatchIf method")
            }
            #[cfg(test)]
            ArrowMethod::Add => {
                todo!("Implement Add method")
            }
            #[cfg(test)]
            ArrowMethod::Sub => {
                todo!("Implement Sub method")
            }
            #[cfg(test)]
            ArrowMethod::Mul => {
                todo!("Implement Mul method")
            }
            #[cfg(test)]
            ArrowMethod::Div => {
                todo!("Implement Div method")
            }
            #[cfg(test)]
            ArrowMethod::Mod => {
                todo!("Implement Mod method")
            }
            #[cfg(test)]
            ArrowMethod::Has => {
                todo!("Implement Has method")
            }
            #[cfg(test)]
            ArrowMethod::Get => {
                todo!("Implement Get method")
            }
            #[cfg(test)]
            ArrowMethod::Keys => {
                todo!("Implement Keys method")
            }
        }
    }

    fn resolve_sub_selection(
        &self,
        sub_selection: &SubSelection,
        input_shape: &Shape,
    ) -> Result<Shape, Vec<DiagnosticWithUrl>> {
        let mut attributes =
            IndexMap::with_capacity_and_hasher(sub_selection.selections.len(), Default::default());
        let mut diagnostics = Vec::new();
        for named_selection in &sub_selection.selections {
            match self.resolve_named_selection(named_selection, input_shape) {
                Ok(new_attributes) => {
                    attributes.extend(new_attributes);
                }
                Err(new_messages) => {
                    diagnostics.extend(new_messages);
                }
            }
        }
        if diagnostics.is_empty() {
            Ok(Shape::Object {
                attributes,
                locations: input_shape
                    .locations()
                    .into_iter()
                    .chain(once(self.location(sub_selection)))
                    .collect(),
            })
        } else {
            Err(diagnostics)
        }
    }

    fn resolve_named_selection(
        &self,
        named_selection: &NamedSelection,
        input_shape: &Shape,
    ) -> Result<Vec<(String, Shape)>, Vec<DiagnosticWithUrl>> {
        match named_selection {
            NamedSelection::Group(alias, sub_selection) => {
                let output_shape = self.resolve_sub_selection(sub_selection, input_shape)?;
                Ok(vec![(alias.name().to_string(), output_shape)])
            }
            NamedSelection::Field(alias, key, sub_selection) => {
                let mut shape = self.get_key(input_shape, key)?;
                let field_name = alias
                    .as_ref()
                    .map(|alias| alias.name().to_string())
                    .unwrap_or_else(|| key.as_string());
                if let Some(sub_selection) = sub_selection {
                    shape = self.resolve_sub_selection(sub_selection, &shape)?;
                }
                Ok(vec![(field_name, shape)])
            }
            NamedSelection::Path(alias, PathSelection { path }) => {
                if let Some(alias) = alias {
                    let field_name = alias.name().to_string();
                    let shape = self.resolve_path(path, input_shape)?;
                    Ok(vec![(field_name, shape)])
                } else {
                    let shape = self.resolve_path(path, input_shape)?;
                    match shape {
                        Shape::Any(_) => Ok(Vec::new()),
                        Shape::Object { attributes, .. } => Ok(attributes.into_iter().collect()),
                        // In practice, this should be caught by the parser, but we have no way to guarantee that yet
                        other => Err(DiagnosticWithUrl::for_locations(
                            other
                                .locations()
                                .into_iter()
                                .chain(once(self.location(path))),
                            format!("Expected an object, got {other:?}.", other = other),
                            DiagnosticSeverity::ERROR,
                        )),
                    }
                }
            }
        }
    }

    fn get_key(
        &self,
        input_shape: &Shape,
        key: &WithRange<Key>,
    ) -> Result<Shape, Vec<DiagnosticWithUrl>> {
        input_shape
            .get_key(key.as_str())
            .map_err(|mut diagnostics| {
                let diagnostics_for_mapping: Vec<_> = diagnostics
                    .iter()
                    .map(|diagnostic| diagnostic.diagnostic.message.clone())
                    .map(|message| {
                        DiagnosticWithUrl::new(
                            self.location(key),
                            message,
                            DiagnosticSeverity::ERROR,
                        )
                    })
                    .collect();
                diagnostics.extend(diagnostics_for_mapping);
                diagnostics
            })
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use serde_json_bytes::json;

    use super::*;

    #[test]
    fn denest_with_sub_selection() {
        let mapping = Document {
            url: Url::parse("file:///path/to/mapping").unwrap(),
            content: "$.object.nested { first second } another",
        };
        let input_shape =
            Shape::from(&json!({"object": {"nested": {"first": 1, "second": 2}}, "another": 3}));
        let output_shape = Shape::from(&json!({"first": 1, "second": 2, "another": 3}));
        let variables = IndexMap::default();
        assert_eq!(
            validate(input_shape, mapping, output_shape, variables).1,
            HashMap::new()
        );
    }
}
