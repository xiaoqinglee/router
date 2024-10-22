use std::fmt::Display;
use std::fmt::Formatter;

use apollo_compiler::collections::IndexMap;
use itertools::Itertools;

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
    pub(super) fn merge(self, other: Self) -> Self {
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

    pub(super) fn get_key(&self, key: &str) -> Result<Self, String> {
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
