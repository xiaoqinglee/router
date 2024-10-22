mod shape;

use std::ops::Range;

use apollo_compiler::collections::IndexMap;
use apollo_compiler::parser::LineColumn;
use line_col::LineColLookup;
use Outcome::*;

pub use self::shape::Shape;
use super::Code;
use super::Message;
use crate::sources::connect::json_selection::ArrowMethod;
use crate::sources::connect::json_selection::KnownVariable;
use crate::sources::connect::json_selection::LitExpr;
use crate::sources::connect::json_selection::MethodArgs;
use crate::sources::connect::json_selection::NamedSelection;
use crate::sources::connect::json_selection::PathList;
use crate::sources::connect::json_selection::Ranged;
use crate::sources::connect::json_selection::WithRange;
use crate::sources::connect::validation::mapping::shape::Signature;
use crate::sources::connect::JSONSelection;
use crate::sources::connect::Key;
use crate::sources::connect::PathSelection;
use crate::sources::connect::SubSelection;

pub fn validate(
    mapping: &str,
    input_shape: Shape,
    output_shape: Shape,
    variables: IndexMap<String, Shape>,
) -> Outcome {
    let parsed = match JSONSelection::parse(mapping) {
        Ok(parsed) => parsed.1,
        Err(err) => {
            return ParseError(Message {
                code: Code::InvalidJsonSelection,
                message: err.to_string(),
                locations: vec![],
            })
        }
    };

    let line_col = LineColLookup::new(mapping);
    let validator = Validator {
        line_col,
        variables,
    };

    let actual_output_shape = match &parsed {
        JSONSelection::Named(sub_selection) => {
            validator.resolve_sub_selection(sub_selection, &input_shape)
        }
        JSONSelection::Path(PathSelection { path }) => validator.resolve_path(path, &input_shape),
    };

    let actual_output_shape = match actual_output_shape {
        Ok(output_shape) => output_shape,
        Err(messages) => {
            return WithErrors(parsed, messages);
        }
    };

    let messages = actual_output_shape.check_compatibility_with(output_shape);
    if !messages.is_empty() {
        return WithErrors(parsed, messages);
    }
    Success(parsed)
}

#[derive(Debug)]
pub enum Outcome {
    ParseError(Message),
    WithErrors(JSONSelection, Vec<Message>),
    Success(JSONSelection),
}

struct Validator<'a> {
    line_col: LineColLookup<'a>,
    variables: IndexMap<String, Shape>,
}

impl Validator<'_> {
    pub(crate) fn location<T>(&self, range: &impl Ranged<T>) -> Vec<Range<LineColumn>> {
        let Some(range) = range.range() else {
            return Vec::new();
        };
        let (line, column) = self.line_col.get(range.start);
        let start = LineColumn { line, column };
        let (line, column) = self.line_col.get(range.end);
        let end = LineColumn { line, column };

        vec![start..end]
    }

    fn resolve_path(
        &self,
        path: &WithRange<PathList>,
        input_shape: &Shape,
    ) -> Result<Shape, Vec<Message>> {
        let mut shape = match path.as_ref() {
            PathList::Var(variable, tail) => {
                match variable.as_ref() {
                    KnownVariable::This | KnownVariable::Args | KnownVariable::Config => {
                        let Some(var_shape) = self.variables.get(variable.as_str()) else {
                            return Err(vec![Message {
                                code: Code::UnsupportedVariableType,
                                message: format!(
                                    "Variable `{variable}` was not set.",
                                    variable = variable.as_str()
                                ),
                                locations: self.location(variable),
                            }]);
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
                let shape = self.resolve_literal(input_shape, expr)?;
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
        shape.source_locations = self.location(path);
        Ok(shape)
    }

    fn resolve_literal(
        &self,
        input_shape: &Shape,
        lit: &WithRange<LitExpr>,
    ) -> Result<Shape, Vec<Message>> {
        let signature = match lit.as_ref() {
            LitExpr::String(_) => Signature::String,
            LitExpr::Number(_) => Signature::Number,
            LitExpr::Bool(_) => Signature::Bool,
            LitExpr::Null => Signature::Nullable(Box::new(Signature::Any)),
            LitExpr::Object(attributes) => {
                let mut inner =
                    IndexMap::with_capacity_and_hasher(attributes.len(), Default::default());
                for (key, value) in attributes {
                    inner.insert(key.as_string(), self.resolve_literal(input_shape, value)?);
                }
                Signature::Object(inner)
            }
            LitExpr::Array(inner) => {
                let mut signature = Signature::Any;
                for expr in inner {
                    signature = signature.merge(self.resolve_literal(input_shape, expr)?.signature);
                }
                Signature::Array(Box::new(signature))
            }
            LitExpr::Path(PathSelection { path }) => return self.resolve_path(path, input_shape),
        };
        Ok(Shape {
            signature,
            source_locations: vec![],
            mapping_locations: self.location(lit),
        })
    }

    fn resolve_method(
        &self,
        input_shape: &Shape,
        name: &WithRange<String>,
        args: Option<MethodArgs>,
    ) -> Result<Shape, Vec<Message>> {
        let Some(method) = ArrowMethod::lookup(name.as_str()) else {
            return Err(vec![Message {
                code: Code::InvalidJsonSelection,
                message: format!("Method `{name}` not found.", name = name.as_str()),
                locations: self.location(name),
            }]);
        };
        let args = args;
        let require_one_arg = |args: Option<MethodArgs>| {
            let Some(args) = args else {
                return Err(vec![Message {
                    code: Code::InvalidJsonSelection,
                    message: format!(
                        "`->{method}` requires exactly one argument.",
                        method = name.as_str()
                    ),
                    locations: self.location(name),
                }]);
            };
            if args.args.len() != 1 {
                return Err(vec![Message {
                    code: Code::InvalidJsonSelection,
                    message: format!(
                        "`->{method}` requires exactly one argument.",
                        method = name.as_str()
                    ),
                    locations: self.location(&args),
                }]);
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
                if let Signature::Array(_) = input_shape.signature {
                    inner_shape.signature = Signature::Array(Box::new(inner_shape.signature));
                }
                Ok(inner_shape)
            }
            ArrowMethod::First | ArrowMethod::Last => {
                if let Some(args) = args {
                    return Err(vec![Message {
                        code: Code::InvalidJsonSelection,
                        message: format!("`->{name}` takes no arguments.", name = name.as_str()),
                        locations: self.location(&args),
                    }]);
                }
                match &input_shape.signature {
                    Signature::Array(inner) => {
                        let mut shape = input_shape.clone();
                        shape.signature = *inner.clone();
                        Ok(shape)
                    }
                    Signature::Any | Signature::String => Ok(input_shape.clone()),
                    _ => Err(vec![Message {
                        code: Code::InvalidJsonSelection,
                        message: format!(
                            "`->{name}` can only be applied to an array or string.",
                            name = name.as_str()
                        ),
                        locations: self.location(name),
                    }]),
                }
            }
            ArrowMethod::Slice => {
                match &input_shape.signature {
                    Signature::Array(_) => Ok(input_shape.clone()),
                    Signature::Any | Signature::String => Ok(input_shape.clone()),
                    _ => Err(vec![Message {
                        code: Code::InvalidJsonSelection,
                        message: format!(
                            "`->{name}` can only be applied to an array or string.",
                            name = name.as_str()
                        ),
                        locations: self.location(name),
                    }]),
                }
                // TODO: check arguments
            }
            ArrowMethod::Size => match &input_shape.signature {
                Signature::Array(_) | Signature::Object(_) | Signature::Any => Ok(Shape {
                    signature: Signature::Number,
                    source_locations: Vec::new(),
                    mapping_locations: self.location(name),
                }),
                _ => Err(vec![Message {
                    code: Code::InvalidJsonSelection,
                    message: format!(
                        "`->{name}` can only be applied to an array or object.",
                        name = name.as_str()
                    ),
                    locations: self.location(name),
                }]),
            },
            ArrowMethod::Match => {
                let arg = require_one_arg(args)?;
                let LitExpr::Array(cases) = arg.as_ref() else {
                    return Err(vec![Message {
                        code: Code::InvalidJsonSelection,
                        message: format!(
                            "`->{name}` requires an array of pairs.",
                            name = name.as_str()
                        ),
                        locations: self.location(&arg),
                    }]);
                };
                let mut shape = Shape {
                    signature: Signature::Any,
                    source_locations: input_shape.source_locations.clone(),
                    mapping_locations: self.location(&arg),
                };
                if cases.is_empty() {
                    return Err(vec![Message {
                        code: Code::InvalidJsonSelection,
                        message: format!(
                            "`->{name}` requires at least one pair.",
                            name = name.as_str()
                        ),
                        locations: self.location(&arg),
                    }]);
                }
                for case in cases {
                    let LitExpr::Array(case_pair) = case.as_ref() else {
                        return Err(vec![Message {
                            code: Code::InvalidJsonSelection,
                            message: format!(
                                "`->{name}` requires an array of pairs.",
                                name = name.as_str()
                            ),
                            locations: self.location(case),
                        }]);
                    };
                    if case_pair.len() != 2 {
                        return Err(vec![Message {
                            code: Code::InvalidJsonSelection,
                            message: format!(
                                "`->{name}` requires an array of pairs.",
                                name = name.as_str()
                            ),
                            locations: self.location(case),
                        }]);
                    }
                    shape = shape.merge(self.resolve_literal(input_shape, &case_pair[1])?);
                }
                Ok(shape)
            }
            ArrowMethod::Entries => {
                if let Some(args) = args {
                    return Err(vec![Message {
                        code: Code::InvalidJsonSelection,
                        message: format!("`->{name}` takes no arguments.", name = name.as_str()),
                        locations: self.location(&args),
                    }]);
                }
                match &input_shape.signature {
                    // TODO: better types
                    Signature::Object(_) | Signature::Any => Ok(Shape {
                        signature: Signature::Array(Box::new(Signature::Any)),
                        source_locations: input_shape.source_locations.clone(),
                        mapping_locations: self.location(name),
                    }),
                    _ => Err(vec![Message {
                        code: Code::InvalidJsonSelection,
                        message: format!(
                            "`->{name}` can only be applied to an object.",
                            name = name.as_str()
                        ),
                        locations: self.location(name),
                    }]),
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
    ) -> Result<Shape, Vec<Message>> {
        let mut output_attributes =
            IndexMap::with_capacity_and_hasher(sub_selection.selections.len(), Default::default());
        let mut messages = Vec::new();
        for named_selection in &sub_selection.selections {
            match self.resolve_named_selection(named_selection, input_shape) {
                Ok(new_attributes) => {
                    output_attributes.extend(new_attributes);
                }
                Err(new_messages) => {
                    messages.extend(new_messages);
                }
            }
        }
        if messages.is_empty() {
            Ok(Shape {
                signature: Signature::Object(output_attributes),
                source_locations: input_shape.source_locations.clone(),
                mapping_locations: self.location(sub_selection),
            })
        } else {
            Err(messages)
        }
    }

    fn resolve_named_selection(
        &self,
        named_selection: &NamedSelection,
        input_shape: &Shape,
    ) -> Result<Vec<(String, Shape)>, Vec<Message>> {
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
                    match shape.signature {
                        Signature::Any => Ok(Vec::new()),
                        Signature::Object(attributes_to_flatten) => {
                            Ok(attributes_to_flatten.into_iter().collect())
                        }
                        // In practice, this should be caught by the parser, but we have no way to guarantee that yet
                        other => Err(vec![Message {
                            code: Code::InvalidJsonSelection,
                            message: format!("Expected an object, got {other:?}.", other = other),
                            locations: self.location(path),
                        }]),
                    }
                }
            }
        }
    }

    fn get_key(&self, input_shape: &Shape, key: &WithRange<Key>) -> Result<Shape, Vec<Message>> {
        input_shape.get_key(key.as_str()).map_err(|message| {
            vec![Message {
                code: Code::InvalidJsonSelection,
                message,
                locations: self.location(key),
            }]
        })
    }
}

#[cfg(test)]
mod tests {
    use serde_json_bytes::json;

    use super::*;

    #[test]
    fn denest_with_sub_selection() {
        let mapping = "$.object.nested { first second } another";
        let input_shape =
            Shape::from(&json!({"object": {"nested": {"first": 1, "second": 2}}, "another": 3}));
        let output_shape = Shape::from(&json!({"first": 1, "second": 2, "another": 3}));
        let variables = IndexMap::default();
        match validate(mapping, input_shape, output_shape, variables) {
            Success(_) => {}
            other => panic!("Expected Success, got {other:?}"),
        }
    }
}
