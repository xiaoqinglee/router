mod shape;

use std::iter::once;
use std::ops::Range;

use apollo_compiler::collections::IndexMap;
use apollo_compiler::parser::LineColumn;
use line_col::LineColLookup;

pub use self::shape::Shape;
use super::Severity;
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
    pub name: String,
    pub content: &'a str,
}

// TODO: merge this with `Message` used by other validations
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Diagnostic {
    pub message: String,
    pub locations: Vec<Location>,
    pub severity: Severity,
}

impl Diagnostic {
    fn new(location: Location, message: String, severity: Severity) -> Self {
        Self {
            message,
            locations: vec![location],
            severity,
        }
    }

    fn for_locations<I>(locations: I, message: String, severity: Severity) -> Self
    where
        I: IntoIterator<Item = Location>,
    {
        Self {
            message,
            locations: locations.into_iter().collect(),
            severity,
        }
    }

    pub fn new_simple(document: String, range: Option<Range<LineColumn>>, message: String) -> Self {
        Self {
            message,
            locations: vec![Location { document, range }],
            severity: Severity::Error,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Location {
    /// An arbitrary identifier for which file the range is associated with
    pub document: String,
    /// 1-indexed, end-exclusive range within `file` (if any)
    pub range: Option<Range<LineColumn>>,
}

pub fn validate(
    input_shape: Shape,
    mapping: Document,
    output_shape: Shape,
    variables: IndexMap<String, Shape>,
) -> (Option<JSONSelection>, Vec<Diagnostic>) {
    let line_col = LineColLookup::new(mapping.content);
    let parsed = match JSONSelection::parse(mapping.content) {
        Ok((_, parsed)) => parsed,
        Err(err) => {
            let diagnostic = Diagnostic::new_simple(mapping.name, None, err.to_string());
            return (None, vec![diagnostic]);
        }
    };

    let validator = Validator {
        line_col,
        variables,
        mapping_name: mapping.name,
    };

    let actual_output_shape = match &parsed {
        JSONSelection::Named(sub_selection) => {
            validator.resolve_sub_selection(sub_selection, &input_shape)
        }
        JSONSelection::Path(PathSelection { path }) => validator.resolve_path(path, &input_shape),
    };

    let actual_output_shape = match actual_output_shape {
        Ok(output_shape) => output_shape,
        Err(diagnostics) => return (Some(parsed), diagnostics),
    };

    let diagnostics = actual_output_shape.check_compatibility_with(output_shape);
    (Some(parsed), diagnostics)
}

struct Validator<'a> {
    line_col: LineColLookup<'a>,
    mapping_name: String,
    variables: IndexMap<String, Shape>,
}

impl Validator<'_> {
    pub(crate) fn location<T>(&self, ranged: &impl Ranged<T>) -> Location {
        let range = ranged.range().map(|range| {
            let (line, column) = self.line_col.get(range.start);
            let start = LineColumn { line, column };
            let (line, column) = self.line_col.get(range.end);
            let end = LineColumn { line, column };
            start..end
        });

        Location {
            document: self.mapping_name.clone(),
            range,
        }
    }

    fn resolve_path(
        &self,
        path: &WithRange<PathList>,
        input_shape: &Shape,
    ) -> Result<Shape, Vec<Diagnostic>> {
        let shape = match path.as_ref() {
            PathList::Var(variable, tail) => {
                match variable.as_ref() {
                    KnownVariable::This | KnownVariable::Args | KnownVariable::Config => {
                        let Some(var_shape) = self.variables.get(variable.as_str()) else {
                            return Err(vec![Diagnostic::new(
                                self.location(variable),
                                format!(
                                    "Variable `{variable}` was not set.",
                                    variable = variable.as_str()
                                ),
                                Severity::Error,
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
                let shape = arrayify(
                    input_shape,
                    self.get_key(input_shape, key).map_err(|diag| vec![diag])?,
                );
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
    ) -> Result<Shape, Vec<Diagnostic>> {
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
    ) -> Result<Shape, Vec<Diagnostic>> {
        let Some(method) = ArrowMethod::lookup(name.as_str()) else {
            return Err(vec![Diagnostic::new(
                self.location(name),
                format!("Method `{name}` not found.", name = name.as_str()),
                Severity::Error,
            )]);
        };
        let require_one_arg = |args: Option<MethodArgs>| {
            let Some(args) = args else {
                return Err(vec![Diagnostic::new(
                    self.location(name),
                    format!(
                        "`->{method}` requires exactly one argument.",
                        method = name.as_str()
                    ),
                    Severity::Error,
                )]);
            };
            if args.args.len() != 1 {
                return Err(vec![Diagnostic::new(
                    self.location(&args),
                    format!(
                        "`->{method}` requires exactly one argument.",
                        method = name.as_str()
                    ),
                    Severity::Error,
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
                    return Err(vec![Diagnostic::new(
                        self.location(&args),
                        format!("`->{name}` takes no arguments.", name = name.as_str()),
                        Severity::Error,
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
                            Diagnostic::new(
                                location,
                                format!(
                                    "`->{name}` can only be applied to an array or string.",
                                    name = name.as_str()
                                ),
                                Severity::Error,
                            )
                        })
                        .collect()),
                }
            }
            ArrowMethod::Slice => {
                match &input_shape {
                    Shape::Array { .. } => Ok(input_shape.clone()),
                    Shape::Any(_) | Shape::String(_) => Ok(input_shape.clone()),
                    _ => Err(vec![Diagnostic::for_locations(
                        input_shape
                            .locations()
                            .into_iter()
                            .chain(once(self.location(name))),
                        format!(
                            "`->{name}` can only be applied to an array or string.",
                            name = name.as_str()
                        ),
                        Severity::Error,
                    )]),
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
                    _ => Err(vec![Diagnostic::for_locations(
                        locations,
                        format!(
                            "`->{name}` can only be applied to an array or object.",
                            name = name.as_str()
                        ),
                        Severity::Error,
                    )]),
                }
            }
            ArrowMethod::Match => {
                let mut locations = input_shape.locations();
                locations.push(self.location(name));
                let args = args.map(|args| args.args).unwrap_or_default();
                if args.is_empty() {
                    return Err(vec![Diagnostic::for_locations(
                        locations.into_iter(),
                        format!(
                            "`->{name}` requires at least one pair.",
                            name = name.as_str()
                        ),
                        Severity::Error,
                    )]);
                }
                let mut shape = Shape::Any(locations);
                for case in args {
                    let LitExpr::Array(case_pair) = case.as_ref() else {
                        return Err(vec![Diagnostic::new(
                            self.location(&case),
                            format!(
                                "`Argument to ->{name}` must be pairs of [pattern, value].",
                                name = name.as_str()
                            ),
                            Severity::Error,
                        )]);
                    };
                    if case_pair.len() != 2 {
                        return Err(vec![Diagnostic::new(
                            self.location(&case),
                            format!(
                                "`Argument to ->{name}` must be pairs of [pattern, value].",
                                name = name.as_str()
                            ),
                            Severity::Error,
                        )]);
                    }
                    shape = shape.merge(self.resolve_literal(input_shape, &case_pair[1])?);
                }
                Ok(shape)
            }
            ArrowMethod::Entries => {
                if let Some(args) = args {
                    return Err(vec![Diagnostic::new(
                        self.location(&args),
                        format!("`->{name}` takes no arguments.", name = name.as_str()),
                        Severity::Error,
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
                    _ => Err(vec![Diagnostic::for_locations(
                        input_shape
                            .locations()
                            .into_iter()
                            .chain(once(self.location(name))),
                        format!(
                            "`->{name}` can only be applied to an object.",
                            name = name.as_str()
                        ),
                        Severity::Error,
                    )]),
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
    ) -> Result<Shape, Vec<Diagnostic>> {
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
            Ok(arrayify(
                input_shape,
                Shape::Object {
                    attributes,
                    locations: input_shape
                        .locations()
                        .into_iter()
                        .chain(once(self.location(sub_selection)))
                        .collect(),
                },
            ))
        } else {
            Err(diagnostics)
        }
    }

    fn resolve_named_selection(
        &self,
        named_selection: &NamedSelection,
        input_shape: &Shape,
    ) -> Result<Vec<(String, Shape)>, Vec<Diagnostic>> {
        match named_selection {
            NamedSelection::Group(alias, sub_selection) => {
                let output_shape = self.resolve_sub_selection(sub_selection, input_shape)?;
                Ok(vec![(alias.name().to_string(), output_shape)])
            }
            NamedSelection::Field(alias, key, sub_selection) => {
                let mut shape = self.get_key(input_shape, key).map_err(|diag| vec![diag])?;
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
                        other => Err(vec![Diagnostic::for_locations(
                            other
                                .locations()
                                .into_iter()
                                .chain(once(self.location(path))),
                            format!("Expected an object, got {other:?}.", other = other),
                            Severity::Error,
                        )]),
                    }
                }
            }
        }
    }

    fn get_key(&self, input_shape: &Shape, key: &WithRange<Key>) -> Result<Shape, Diagnostic> {
        input_shape
            .get_key(key.as_str())
            .map_err(|mut diagnostic| {
                diagnostic.locations.push(self.location(key));
                diagnostic
            })
            .map(|mut shape| {
                shape.add_location(self.location(key));
                shape
            })
    }
}

/// In some, but not all cases, JSONSelection magically makes the output an array if the input
/// was an array. Like, getting the key of an array of object results in an array of values.
/// This handles that.
fn arrayify(input_shape: &Shape, result_shape: Shape) -> Shape {
    if input_shape.is_array() && !result_shape.is_array() {
        Shape::Array {
            locations: result_shape.locations(),
            inner: Box::new(result_shape),
        }
    } else {
        result_shape
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
            name: "mapping".to_string(),
            content: "$.object.nested { first second } another",
        };
        let input_shape =
            Shape::from(&json!({"object": {"nested": {"first": 1, "second": 2}}, "another": 3}));
        let output_shape = Shape::from(&json!({"first": 1, "second": 2, "another": 3}));
        let variables = IndexMap::default();
        assert_eq!(
            validate(input_shape, mapping, output_shape, variables).1,
            Vec::new(),
        );
    }
}
