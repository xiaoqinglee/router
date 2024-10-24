use std::fmt::Display;
use std::fmt::Formatter;

use apollo_compiler::collections::IndexMap;
use itertools::Itertools;
use lsp_types::DiagnosticSeverity;
use lsp_types::Location;
use lsp_types::Position;
use lsp_types::Range;
use url::Url;

use super::DiagnosticWithUrl;

type Locations = Vec<Location>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Shape {
    /// We have no idea what shape this will be, so we can't validate it.
    Any(Locations),
    Bool(Locations),
    Number(Locations),
    String(Locations),
    Array {
        locations: Locations,
        inner: Box<Self>,
    },
    Object {
        locations: Locations,
        attributes: IndexMap<String, Self>,
    },
    /// Either the inner shape or null.
    Nullable(Box<Self>),
    /// One of a number of shapes.
    Union(Vec<Self>),
}

impl Shape {
    pub(super) fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::Any(_), typed) | (typed, Self::Any(_)) => typed,
            (Self::Bool(first), Self::Bool(second)) => Self::Bool(combine(first, second)),
            (Self::Number(first), Self::Number(second)) => Self::Number(combine(first, second)),
            (Self::String(first), Self::String(second)) => Self::String(combine(first, second)),
            (
                Self::Array { inner, locations },
                Self::Array {
                    inner: other,
                    locations: other_locations,
                },
            ) => Self::Array {
                inner: Box::new(inner.merge(*other)),
                locations: combine(locations, other_locations),
            },
            (
                Self::Object {
                    attributes,
                    locations,
                },
                Self::Object {
                    attributes: mut other_attributes,
                    locations: other_locations,
                },
            ) => {
                let mut attributes: IndexMap<_, _> = attributes
                    .into_iter()
                    .map(|(key, mut value)| {
                        if let Some(other_inner) = other_attributes.shift_remove(&key) {
                            value = value.merge(other_inner);
                        }
                        (key, value)
                    })
                    .collect();
                attributes.extend(other_attributes);

                Self::Object {
                    attributes,
                    locations: combine(locations, other_locations),
                }
            }
            (Self::Nullable(first), Self::Nullable(second)) => {
                Self::Nullable(Box::from(first.merge(*second)))
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

    pub(super) fn get_key(&self, key: &str) -> Result<Self, Vec<DiagnosticWithUrl>> {
        match self {
            Self::Any(_) => Ok(self.clone()),
            Self::Bool(locations) | Self::Number(locations) | Self::String(locations) => {
                Err(locations
                    .iter()
                    .map(|location| {
                        DiagnosticWithUrl::new(
                            location.clone(),
                            format!("Can't get key `{key}` from a {self}."),
                            DiagnosticSeverity::ERROR,
                        )
                    })
                    .collect())
            }
            Self::Array { inner, .. } => inner.get_key(key),
            Self::Object {
                attributes,
                locations,
            } => attributes.get(key).cloned().ok_or_else(|| {
                locations
                    .iter()
                    .map(|location| {
                        DiagnosticWithUrl::new(
                            location.clone(),
                            format!("Key `{key}` not found."),
                            DiagnosticSeverity::ERROR,
                        )
                    })
                    .collect()
            }),
            Self::Nullable(inner) => inner.get_key(key),
            Self::Union(inners) => {
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

    pub(super) fn check_compatibility_with(self, other: Self) -> Vec<DiagnosticWithUrl> {
        use Shape::*;
        match (self, other) {
            (Any(_), _)
            | (_, Any(_))
            | (Bool(_), Bool(_))
            | (Number(_), Number(_))
            | (String(_), String(_)) => vec![],
            (
                Array { inner, .. },
                Array {
                    inner: other_inner, ..
                },
            ) => inner.check_compatibility_with(*other_inner),
            (Union(_), Union(_)) => {
                // TODO: type unions are hard
                vec![]
            }
            (
                Object { attributes, .. },
                Object {
                    attributes: mut other_attributes,
                    ..
                },
            ) => {
                let mut diagnostics = vec![];
                // Check all the attributes we _do_ have
                for (key, value) in attributes {
                    if let Some(other_value) = other_attributes.shift_remove(&key) {
                        diagnostics.extend(
                            value.check_compatibility_with(other_value).into_iter().map(
                                |mut diagnostic| {
                                    diagnostic.diagnostic.message = format!(
                                        "Key `{key}`: {message}",
                                        message = diagnostic.diagnostic.message
                                    );
                                    diagnostic
                                },
                            ),
                        );
                    } else {
                        diagnostics.extend(value.locations().iter().map(|location| {
                            DiagnosticWithUrl::new(
                                location.clone(),
                                format!("Extra key `{key}` found, but not required."),
                                DiagnosticSeverity::WARNING,
                            )
                        }));
                    }
                }
                for (key, value) in other_attributes {
                    diagnostics.extend(value.locations().iter().map(|location| {
                        DiagnosticWithUrl::new(
                            location.clone(),
                            format!("Expected key `{key}` not found."),
                            DiagnosticSeverity::ERROR,
                        )
                    }));
                }
                diagnostics
            }
            (Nullable(inner), other) | (other, Nullable(inner)) => {
                inner.check_compatibility_with(other)
            }
            (actual, expected) => actual
                .locations()
                .into_iter()
                .chain(expected.locations())
                .map(|location| {
                    DiagnosticWithUrl::new(
                        location,
                        format!("Expected {expected}, found {actual}."),
                        DiagnosticSeverity::ERROR,
                    )
                })
                .collect(),
        }
    }

    pub(crate) fn locations(&self) -> Vec<Location> {
        match self {
            Self::Any(locations) => locations.clone(),
            Self::Bool(locations) => locations.clone(),
            Self::Number(locations) => locations.clone(),
            Self::String(locations) => locations.clone(),
            Self::Array { locations, .. } => locations.clone(),
            Self::Object { locations, .. } => locations.clone(),
            Self::Nullable(inner) => inner.locations(),
            Self::Union(inners) => inners.iter().flat_map(Self::locations).collect(),
        }
    }

    pub(crate) fn add_location(&mut self, location: Location) {
        match self {
            Self::Any(locations)
            | Self::Bool(locations)
            | Self::Number(locations)
            | Self::String(locations)
            | Self::Array { locations, .. }
            | Self::Object { locations, .. } => locations.push(location),
            Self::Nullable(inner) => inner.add_location(location),
            Self::Union(inners) => inners
                .iter_mut()
                .for_each(|inner| inner.add_location(location.clone())),
        }
    }

    pub(super) fn replace_locations(&mut self, locations: Vec<Location>) {
        match self {
            Self::Any(old_locations)
            | Self::Bool(old_locations)
            | Self::Number(old_locations)
            | Self::String(old_locations)
            | Self::Array {
                locations: old_locations,
                ..
            }
            | Self::Object {
                locations: old_locations,
                ..
            } => *old_locations = locations,
            Self::Nullable(inner) => inner.replace_locations(locations),
            Self::Union(inners) => inners
                .iter_mut()
                .for_each(|inner| inner.replace_locations(locations.clone())),
        }
    }
}

impl From<&serde_json_bytes::Value> for Shape {
    fn from(value: &serde_json_bytes::Value) -> Self {
        let locations = Vec::new();
        match value {
            // If the value was null, we have no way of knowing what the true shape is.
            serde_json_bytes::Value::Null => Self::Nullable(Box::new(Self::Any(locations))),
            serde_json_bytes::Value::Bool(_) => Self::Bool(locations),
            serde_json_bytes::Value::Number(_) => Self::Number(locations),
            serde_json_bytes::Value::String(_) => Self::String(locations),
            serde_json_bytes::Value::Array(values) => {
                let mut inner = Self::Any(locations.clone());
                for value in values {
                    inner = inner.merge(Shape::from(value));
                }
                Self::Array {
                    inner: Box::new(inner),
                    locations,
                }
            }
            serde_json_bytes::Value::Object(object) => {
                let mut attributes =
                    IndexMap::with_capacity_and_hasher(object.len(), Default::default());
                for (key, value) in object {
                    attributes.insert(key.as_str().to_string(), Shape::from(value));
                }
                Self::Object {
                    attributes,
                    locations,
                }
            }
        }
    }
}

impl Shape {
    pub fn from_json(url: Url, value: spanned_json_parser::SpannedValue) -> Self {
        use spanned_json_parser::Value::*;
        let location = Location {
            uri: url.clone(),
            range: Range {
                start: Position {
                    line: (value.start.line - 1) as u32,
                    character: (value.start.col - 1) as u32,
                },
                end: Position {
                    line: (value.end.line - 1) as u32,
                    // Their end col is actually already off by one since it's inclusive not exclusive
                    character: value.end.col as u32,
                },
            },
        };
        let locations = vec![location];
        match value.value {
            Null => Self::Nullable(Box::new(Self::Any(locations))),
            Bool(_) => Self::Bool(locations),
            Number(_) => Self::Number(locations),
            String(_) => Self::String(locations),
            Array(values) => {
                let mut inner = Self::Any(Vec::new());
                for value in values {
                    inner = inner.merge(Self::from_json(url.clone(), value));
                }
                Self::Array {
                    inner: Box::new(inner),
                    locations,
                }
            }
            Object(object) => {
                let mut attributes =
                    IndexMap::with_capacity_and_hasher(object.len(), Default::default());
                for (key, value) in object {
                    attributes.insert(key, Shape::from_json(url.clone(), value));
                }
                Self::Object {
                    attributes,
                    locations,
                }
            }
        }
    }
}

impl Display for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any(_) => write!(f, "Any"),
            Self::Bool(_) => write!(f, "Bool"),
            Self::Number(_) => write!(f, "Number"),
            Self::String(_) => write!(f, "String"),
            Self::Array { inner, .. } => write!(f, "[{}]", inner),
            Self::Object { attributes, .. } => {
                write!(f, "{{")?;
                let mut attributes = attributes.iter();
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

impl Default for Shape {
    fn default() -> Self {
        Self::Any(Vec::new())
    }
}

fn combine(first: Locations, second: Locations) -> Locations {
    first.into_iter().chain(second).dedup().collect()
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
        let shape = Shape::from_json(Url::parse("file://test").unwrap(), value);
        assert_eq!(
            shape.locations(),
            vec![Location {
                uri: Url::parse("file://test").unwrap(),
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0
                    },
                    end: Position {
                        line: 0,
                        character: 15
                    }
                }
            }]
        );
        let Shape::Object { attributes, .. } = shape else {
            panic!("Expected an object, found {:?}", shape);
        };
        assert_eq!(attributes.len(), 2);
        assert_eq!(
            attributes["a"].locations(),
            vec![Location {
                uri: Url::parse("file://test").unwrap(),
                range: Range {
                    start: Position {
                        line: 0,
                        character: 6
                    },
                    end: Position {
                        line: 0,
                        character: 7
                    }
                }
            }]
        );
        assert_eq!(
            attributes["b"].locations(),
            vec![Location {
                uri: Url::parse("file://test").unwrap(),
                range: Range {
                    start: Position {
                        line: 0,
                        character: 10
                    },
                    end: Position {
                        line: 0,
                        character: 13
                    }
                }
            }]
        );
    }
}
