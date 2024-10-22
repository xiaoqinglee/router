use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::Range;

use apollo_compiler::collections::IndexMap;
use apollo_compiler::parser::LineColumn;
use itertools::Itertools;
use line_col::LineColLookup;
use Outcome::*;

use super::Code;
use super::Message;
use crate::sources::connect::json_selection::ArrowMethod;
use crate::sources::connect::json_selection::KnownVariable;
use crate::sources::connect::json_selection::LitExpr;
use crate::sources::connect::json_selection::MethodArgs;
use crate::sources::connect::json_selection::PathList;
use crate::sources::connect::json_selection::Ranged;
use crate::sources::connect::json_selection::WithRange;
use crate::sources::connect::JSONSelection;
use crate::sources::connect::PathSelection;

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

    let output_shape = match &parsed {
        JSONSelection::Named(_) => Ok(Shape::Any), // TODO: validate_sub_selection(sub_selection, arg),
        JSONSelection::Path(PathSelection { path }) => validator.resolve_path(path, &input_shape),
    };

    let actual_output_shape = match output_shape {
        Ok(output_shape) => output_shape,
        Err(messages) => {
            return WithErrors(parsed, messages);
        }
    };

    // TODO: check actual_output_shape against output_shape
    Success(parsed)
}

pub enum Outcome {
    ParseError(Message),
    WithErrors(JSONSelection, Vec<Message>),
    Success(JSONSelection),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Shape {
    /// We have no idea what shape this will be, so we can't validate it.
    Any,
    Bool,
    Number,
    String,
    Array(Box<Shape>),
    Object(IndexMap<String, Shape>),
    /// Either the inner shape or null.
    Nullable(Box<Shape>),
    /// One of a number of shapes.
    Union(Vec<Shape>),
}

impl Shape {
    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Shape::Any, typed) | (typed, Shape::Any) => typed,
            (Shape::Bool, Shape::Bool) => Shape::Bool,
            (Shape::Number, Shape::Number) => Shape::Number,
            (Shape::String, Shape::String) => Shape::String,
            (Shape::Array(inner), Shape::Array(other_inner)) => {
                Shape::Array(Box::new(inner.merge(*other_inner)))
            }
            (Shape::Object(inner), Shape::Object(mut other_inner)) => {
                let mut merged =
                    IndexMap::with_capacity_and_hasher(inner.len(), Default::default());
                for (key, value) in inner {
                    let other_value = other_inner.shift_remove(&key);
                    let value = value.merge(other_value.unwrap_or(Shape::Any));
                    merged.insert(key, value);
                }
                for (key, value) in other_inner {
                    merged.insert(key, value);
                }
                Shape::Object(merged)
            }
            (Shape::Nullable(inner), other) | (other, Shape::Nullable(inner)) => {
                Shape::Nullable(Box::new(inner.merge(other)))
            }
            (Shape::Union(inner), Shape::Union(other)) => {
                Shape::Union(inner.into_iter().chain(other).dedup().collect())
            }
            (Shape::Union(inner), other) | (other, Shape::Union(inner)) => Shape::Union(
                inner
                    .into_iter()
                    .chain(std::iter::once(other))
                    .dedup()
                    .collect(),
            ),
            (first, second) => Shape::Union(vec![first, second]),
        }
    }

    fn get_key(&self, key: &str) -> Result<Self, String> {
        match self {
            Shape::Any => Ok(Shape::Any),
            Shape::Bool | Shape::Number | Shape::String => {
                Err(format!("Can't get key `{key}` from a {self}."))
            }
            Shape::Array(inner) => inner.get_key(key),
            Shape::Object(attributes) => attributes
                .get(key)
                .cloned()
                .ok_or_else(|| format!("Key `{key}` not found in {self}.")),
            Shape::Nullable(inner) => inner.get_key(key),
            Shape::Union(inners) => {
                let mut key_union = Vec::with_capacity(inners.len());
                for inner in inners {
                    match inner.get_key(key) {
                        Ok(inner) => key_union.push(inner.clone()),
                        Err(err) => return Err(err),
                    }
                }
                Ok(Shape::Union(key_union))
            }
        }
    }
}

impl From<&serde_json_bytes::Value> for Shape {
    fn from(value: &serde_json_bytes::Value) -> Self {
        match value {
            // If the value was null, we have no way of knowing what the true shape is.
            serde_json_bytes::Value::Null => Shape::Nullable(Box::new(Shape::Any)),
            serde_json_bytes::Value::Bool(_) => Shape::Bool,
            serde_json_bytes::Value::Number(_) => Shape::Number,
            serde_json_bytes::Value::String(_) => Shape::String,
            serde_json_bytes::Value::Array(values) => {
                let mut inner = Shape::Any;
                for value in values {
                    inner = inner.merge(Shape::from(value));
                }
                Shape::Array(Box::new(inner))
            }
            serde_json_bytes::Value::Object(object) => {
                let mut inner =
                    IndexMap::with_capacity_and_hasher(object.len(), Default::default());
                for (key, value) in object {
                    inner.insert(key.as_str().to_string(), Shape::from(value));
                }
                Shape::Object(inner)
            }
        }
    }
}

impl Display for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Shape::Any => write!(f, "Any"),
            Shape::Bool => write!(f, "Bool"),
            Shape::Number => write!(f, "Number"),
            Shape::String => write!(f, "String"),
            Shape::Array(inner) => write!(f, "[{}]", inner),
            Shape::Object(inner) => {
                write!(f, "{inner:?}")
            }
            Shape::Nullable(inner) => write!(f, "{}?", inner),
            Shape::Union(inner) => {
                write!(f, "(")?;
                write!(f, "{}", inner.iter().format(" | "))?;
                write!(f, ")")
            }
        }
    }
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
        match path.as_ref() {
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
                let shape = input_shape.get_key(key.as_str()).map_err(|message| {
                    vec![Message {
                        code: Code::InvalidJsonSelection,
                        message,
                        locations: self.location(key),
                    }]
                })?;
                self.resolve_path(tail, &shape)
            }
            PathList::Expr(expr, tail) => {
                let shape = self.resolve_literal(input_shape, expr.as_ref())?;
                self.resolve_path(tail, &shape)
            }
            PathList::Method(name, args, tail) => {
                let shape = self.resolve_method(input_shape, name, args.clone())?;
                self.resolve_path(tail, &shape)
            }
            PathList::Selection(_) => {
                // TODO:
                Ok(Shape::Any)
            }
            PathList::Empty => Ok(input_shape.clone()),
        }
    }

    fn resolve_literal(&self, input_shape: &Shape, lit: &LitExpr) -> Result<Shape, Vec<Message>> {
        match lit {
            LitExpr::String(_) => Ok(Shape::String),
            LitExpr::Number(_) => Ok(Shape::Number),
            LitExpr::Bool(_) => Ok(Shape::Bool),
            LitExpr::Null => Ok(Shape::Nullable(Box::new(Shape::Any))),
            LitExpr::Object(attributes) => {
                let mut inner =
                    IndexMap::with_capacity_and_hasher(attributes.len(), Default::default());
                for (key, value) in attributes {
                    inner.insert(
                        key.as_string(),
                        self.resolve_literal(input_shape, value.as_ref())?,
                    );
                }
                Ok(Shape::Object(inner))
            }
            LitExpr::Array(inner) => {
                let mut inner_shape = Shape::Any;
                for expr in inner {
                    inner_shape =
                        inner_shape.merge(self.resolve_literal(input_shape, expr.as_ref())?);
                }
                Ok(Shape::Array(Box::new(inner_shape)))
            }
            LitExpr::Path(PathSelection { path }) => self.resolve_path(path, input_shape),
        }
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
                self.resolve_literal(input_shape, arg.as_ref())
            }
            ArrowMethod::Map => {
                let arg = require_one_arg(args)?;
                let inner_shape = self.resolve_literal(input_shape, arg.as_ref())?;
                if let Shape::Array(_) = input_shape {
                    Ok(Shape::Array(Box::new(inner_shape)))
                } else {
                    Ok(inner_shape)
                }
            }
            ArrowMethod::First | ArrowMethod::Last => {
                if let Some(args) = args {
                    return Err(vec![Message {
                        code: Code::InvalidJsonSelection,
                        message: format!("`->{name}` takes no arguments.", name = name.as_str()),
                        locations: self.location(&args),
                    }]);
                }
                match input_shape {
                    Shape::Array(inner) => Ok(*inner.clone()),
                    Shape::String => Ok(Shape::String),
                    Shape::Any => Ok(Shape::Any),
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
                match input_shape {
                    Shape::Array(_) => Ok(input_shape.clone()),
                    Shape::String => Ok(Shape::String),
                    Shape::Any => Ok(Shape::Any),
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
            ArrowMethod::Size => match input_shape {
                Shape::Array(_) | Shape::Object(_) | Shape::Any => Ok(Shape::Number),
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
                let mut shape = Shape::Any;
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
                match input_shape {
                    // TODO: better types
                    Shape::Object(_) | Shape::Any => Ok(Shape::Array(Box::new(Shape::Any))),
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
            #[cfg(test)]
            ArrowMethod::Values => {
                todo!("Implement Values method")
            }
            #[cfg(test)]
            ArrowMethod::And => {
                todo!("Implement And method")
            }
            #[cfg(test)]
            ArrowMethod::Or => {
                todo!("Implement Or method")
            }
            #[cfg(test)]
            ArrowMethod::Not => {
                todo!("Implement Not method")
            }
        }
    }
}
