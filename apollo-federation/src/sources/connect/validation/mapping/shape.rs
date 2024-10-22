use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::Range;

use apollo_compiler::collections::IndexMap;
use apollo_compiler::parser::LineColumn;
use itertools::Itertools;

use crate::sources::connect::validation::Code;
use crate::sources::connect::validation::Message;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Shape {
    pub(super) signature: Signature,
    /// Where in the source code this shape was found. Like the JSON object or GraphQL SDL
    pub(super) source_locations: Vec<Range<LineColumn>>,
    /// Where in mapping this field was used.
    pub(super) mapping_locations: Vec<Range<LineColumn>>,
}

impl Shape {
    pub(super) fn merge(mut self, other: Self) -> Self {
        self.source_locations.extend(other.source_locations);
        self.mapping_locations.extend(other.mapping_locations);
        Self {
            signature: self.signature.merge(other.signature),
            source_locations: self.source_locations,
            mapping_locations: self.mapping_locations,
        }
    }

    pub(super) fn get_key(&self, key: &str) -> Result<Shape, String> {
        if let Some(shape) = self.signature.get_key(key)? {
            Ok(shape)
        } else {
            // `signature.get_key` was any, we want to preserve location data
            Ok(Shape {
                signature: Signature::Any,
                source_locations: self.source_locations.clone(),
                mapping_locations: self.mapping_locations.clone(),
            })
        }
    }

    pub(super) fn check_compatibility_with(self, other: Shape) -> Vec<Message> {
        self.signature
            .check_compatibility_with(other.signature)
            .into_iter()
            .map(|mut message| {
                if message.locations.is_empty() {
                    // Some signatures come with specific locations, like object keys
                    // Others should use the location of the overall shape
                    message.locations = self
                        .source_locations
                        .iter()
                        .chain(&other.source_locations)
                        .chain(&self.mapping_locations)
                        .chain(&other.mapping_locations)
                        .cloned()
                        .collect()
                }
                message
            })
            .collect()
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(super) enum Signature {
    /// We have no idea what shape this will be, so we can't validate it.
    #[default]
    Any,
    Bool,
    Number,
    String,
    Array(Box<Signature>),
    Object(IndexMap<String, Shape>),
    /// Either the inner shape or null.
    Nullable(Box<Signature>),
    /// One of a number of shapes.
    Union(Vec<Signature>),
}

impl Signature {
    pub(super) fn merge(self, other: Signature) -> Self {
        match (self, other) {
            (Self::Any, typed) | (typed, Self::Any) => typed,
            (Self::Bool, Self::Bool) => Self::Bool,
            (Self::Number, Self::Number) => Self::Number,
            (Self::String, Self::String) => Self::String,
            (Self::Array(inner), Self::Array(other_inner)) => {
                Self::Array(Box::new(inner.merge(*other_inner.clone())))
            }
            (Self::Object(inner), Self::Object(mut other_inner)) => {
                let mut merged =
                    IndexMap::with_capacity_and_hasher(inner.len(), Default::default());
                for (key, mut value) in inner {
                    if let Some(other_inner) = other_inner.shift_remove(&key) {
                        value = value.merge(other_inner);
                    }
                    merged.insert(key, value);
                }
                for (key, value) in other_inner {
                    merged.insert(key, value);
                }
                Self::Object(merged)
            }
            (Self::Nullable(inner), other) | (other, Self::Nullable(inner)) => {
                Self::Nullable(Box::from(inner.merge(other)))
            }
            (Self::Union(inner), Self::Union(other)) => {
                Self::Union(inner.into_iter().chain(other).dedup().collect())
            }
            (Self::Union(inner), other) | (other, Self::Union(inner)) => Self::Union(
                inner
                    .into_iter()
                    .chain(std::iter::once(other))
                    .dedup()
                    .collect(),
            ),
            (first, second) => Self::Union(vec![first, second]),
        }
    }

    pub(super) fn get_key(&self, key: &str) -> Result<Option<Shape>, String> {
        match self {
            Self::Any => Ok(None),
            Self::Bool | Self::Number | Self::String => {
                Err(format!("Can't get key `{key}` from a {self}."))
            }
            Self::Array(inner) => inner.get_key(key),
            Self::Object(attributes) => attributes
                .get(key)
                .cloned()
                .ok_or_else(|| format!("Key `{key}` not found in {self}."))
                .map(Some),
            Self::Nullable(inner) => inner.get_key(key),
            Self::Union(inners) => {
                let mut key_union = Vec::with_capacity(inners.len());
                for inner in inners {
                    match inner.get_key(key) {
                        Ok(Some(inner)) => key_union.push(inner.clone()),
                        Ok(None) => continue,
                        Err(err) => return Err(err),
                    }
                }
                Ok(Some(Shape {
                    // TODO: figure out tracking these locations
                    signature: Signature::Union(
                        key_union.into_iter().map(|inner| inner.signature).collect(),
                    ),
                    source_locations: vec![],
                    mapping_locations: vec![],
                }))
            }
        }
    }

    fn check_compatibility_with(self, other: Self) -> Vec<Message> {
        use Signature::*;
        match (self, other) {
            (Any, _) | (_, Any) | (Bool, Bool) | (Number, Number) | (String, String) => vec![],
            (Array(inner), Array(other_inner)) => inner.check_compatibility_with(*other_inner),
            (Union(_), Union(_)) => {
                // TODO: type unions are hard
                vec![]
            }
            (Object(attributes), Object(mut other_attributes)) => {
                let mut messages = vec![];
                // Check all the attributes we _do_ have
                for (key, value) in attributes {
                    if let Some(other_value) = other_attributes.shift_remove(&key) {
                        messages.extend(
                            value.check_compatibility_with(other_value).into_iter().map(
                                |mut message| {
                                    message.message = format!(
                                        "Key `{key}`: {message}",
                                        message = message.message
                                    );
                                    message
                                },
                            ),
                        );
                    } else {
                        messages.push(Message {
                            message: format!("Extra key `{key}` found, but not required."),
                            code: Code::UnusedValue,
                            locations: vec![],
                        });
                    }
                }
                for (key, _value) in other_attributes {
                    messages.push(Message {
                        message: format!("Expected key `{key}` not found."),
                        code: Code::InvalidJsonSelection,
                        locations: vec![],
                    });
                }
                messages
            }
            (Nullable(inner), other) | (other, Nullable(inner)) => {
                inner.check_compatibility_with(other)
            }
            (actual, expected) => vec![Message {
                message: format!("Expected {expected}, found {actual}."),
                code: Code::InvalidJsonSelection,
                locations: vec![],
            }],
        }
    }
}

impl From<&serde_json_bytes::Value> for Shape {
    fn from(value: &serde_json_bytes::Value) -> Self {
        let signature = match value {
            // If the value was null, we have no way of knowing what the true shape is.
            serde_json_bytes::Value::Null => Signature::Nullable(Box::new(Signature::Any)),
            serde_json_bytes::Value::Bool(_) => Signature::Bool,
            serde_json_bytes::Value::Number(_) => Signature::Number,
            serde_json_bytes::Value::String(_) => Signature::String,
            serde_json_bytes::Value::Array(values) => {
                let mut inner = Signature::Any;
                for value in values {
                    inner = inner.merge(Shape::from(value).signature);
                }
                Signature::Array(Box::new(inner))
            }
            serde_json_bytes::Value::Object(object) => {
                let mut inner =
                    IndexMap::with_capacity_and_hasher(object.len(), Default::default());
                for (key, value) in object {
                    inner.insert(key.as_str().to_string(), Shape::from(value));
                }
                Signature::Object(inner)
            }
        };
        Self {
            signature,
            source_locations: vec![],
            mapping_locations: vec![],
        }
    }
}

impl From<spanned_json_parser::SpannedValue> for Shape {
    fn from(value: spanned_json_parser::SpannedValue) -> Self {
        use spanned_json_parser::Value::*;
        let signature = match value.value {
            Null => Signature::Nullable(Box::new(Signature::Any)),
            Bool(_) => Signature::Bool,
            Number(_) => Signature::Number,
            String(_) => Signature::String,
            Array(values) => {
                let mut inner = Signature::Any;
                for value in values {
                    inner = inner.merge(Shape::from(value).signature);
                }
                Signature::Array(Box::new(inner))
            }
            Object(object) => {
                let mut inner =
                    IndexMap::with_capacity_and_hasher(object.len(), Default::default());
                for (key, value) in object {
                    inner.insert(key, Shape::from(value));
                }
                Signature::Object(inner)
            }
        };
        let location = LineColumn {
            line: value.start.line,
            column: value.start.col,
        }..LineColumn {
            line: value.end.line,
            column: value.end.col + 1,
        };
        Self {
            signature,
            source_locations: vec![location],
            mapping_locations: vec![],
        }
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any => write!(f, "Any"),
            Self::Bool => write!(f, "Bool"),
            Self::Number => write!(f, "Number"),
            Self::String => write!(f, "String"),
            Self::Array(inner) => write!(f, "[{}]", inner),
            Self::Object(inner) => {
                write!(f, "{{")?;
                let mut attributes = inner.iter();
                if let Some((key, value)) = attributes.next() {
                    write!(f, "{key}: {value}")?;
                }
                for (key, value) in attributes {
                    write!(f, ", {}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            Self::Nullable(inner) => write!(f, "{}?", inner),
            Self::Union(inner) => {
                write!(f, "(")?;
                write!(f, "{}", inner.iter().format(" | "))?;
                write!(f, ")")
            }
        }
    }
}

impl Display for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.signature)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use spanned_json_parser::parse;

    use super::*;
    #[test]
    fn spanned_json_locations() {
        let input = r#"{"a": 1, "b": "2"}"#;
        let value = parse(input).unwrap();
        let shape = Shape::from(value);
        assert_eq!(
            shape.source_locations,
            vec![
                LineColumn { line: 1, column: 1 }..LineColumn {
                    line: 1,
                    column: 19
                }
            ]
        );
        let Signature::Object(attributes) = shape.signature else {
            panic!("Expected an object, found {:?}", shape.signature);
        };
        assert_eq!(attributes.len(), 2);
        assert_eq!(
            attributes["a"].source_locations,
            vec![LineColumn { line: 1, column: 7 }..LineColumn { line: 1, column: 8 }]
        );
        assert_eq!(
            attributes["b"].source_locations,
            vec![
                LineColumn {
                    line: 1,
                    column: 15
                }..LineColumn {
                    line: 1,
                    column: 18
                }
            ]
        );
    }
}
