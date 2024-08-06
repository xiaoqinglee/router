use std::fmt::Display;
use std::sync::Arc;

use apollo_compiler::collections::IndexMap;
use apollo_compiler::executable::SelectionSet;
use itertools::Itertools;
use jaq_syn::filter::BinaryOp;
use jaq_syn::filter::Filter;
use jaq_syn::filter::KeyVal;
use jaq_syn::string::Part;
use jaq_syn::Lexer;
use jaq_syn::Main;
use jaq_syn::Parser;
use jaq_syn::Spanned;
pub use json_selection::JSONSelection;
pub use json_selection::JSONSelectionVisitor;
// TODO: use the new visitors for validation
use json_selection::SubSelection;
use serde_json_bytes::Value;

use crate::error::FederationError;
use crate::sources::connect::selection::json_selection::ApplyToError;

mod jq;
mod json_selection;

#[derive(Clone, Debug)]
pub enum Selection {
    Json {
        source: Arc<str>,
        parsed: JSONSelection,
    },
    Jq {
        source: Arc<str>,
        parsed: Main,
    },
}

impl Selection {
    pub(crate) fn group(&self) -> Option<Group> {
        match self {
            Selection::Json { parsed, .. } => parsed.next_subselection().map(Group::Json),
            Selection::Jq { parsed, .. } => jq_group(&parsed.body.0),
        }
    }

    pub fn parse(source: &str) -> Result<Self, FederationError> {
        if source.starts_with("# jq") {
            Self::parse_jq(source)
        } else {
            Self::parse_json_selection(source)
        }
    }

    pub fn parse_json_selection(source: &str) -> Result<Self, FederationError> {
        let (remainder, parsed) = JSONSelection::parse(&source)
            .map_err(|err| FederationError::internal(err.to_string()))?;
        if remainder != "" {
            return Err(FederationError::internal(format!(
                "JSONSelection parsing failed, trailing characters: {remainder}"
            )));
        }
        Ok(Self::Json {
            source: Arc::from(source),
            parsed,
        })
    }

    pub fn parse_jq(selection_str: &str) -> Result<Self, FederationError> {
        let tokens = Lexer::new(selection_str).lex().map_err(|err| {
            FederationError::internal(format!(
                "Invalid jq syntax: {}",
                err.into_iter().map(|err| err.0.as_str()).join("\n")
            ))
        })?;
        let mut parser = Parser::new(&tokens);
        parser
            .module(|parser| parser.term())
            .map_err(|err| {
                FederationError::internal(format!("Invalid jq syntax: {}", err.0.as_str()))
            })
            .map(|module| module.conv(selection_str))
            .map(|main| Self::Jq {
                source: selection_str.into(),
                parsed: main,
            })
    }

    pub fn transform(
        &self,
        selection_set: &SelectionSet,
        json: &Value,
        vars: &IndexMap<String, Value>,
    ) -> (Option<Value>, Vec<TransformError>) {
        match self {
            Selection::Json { parsed, .. } => {
                let transformed_selection = parsed.apply_selection_set(selection_set);
                let (res, errors) = transformed_selection.apply_with_vars(json, vars);
                (res, errors.into_iter().map(TransformError::Json).collect())
            }
            Selection::Jq { parsed, .. } => jq::execute(&json, vars, parsed),
        }
    }
}

fn jq_group(filter: &Filter) -> Option<Group> {
    match filter {
        Filter::Object(keyvals) => Some(Group::Jq(keyvals)),
        Filter::Array(inner) => inner.as_ref().and_then(|filter| jq_group(&filter.0)),
        Filter::Binary(_input, op, filter) => match op {
            BinaryOp::Pipe(None) => jq_group(&filter.0),
            _ => None,
        },
        _ => None,
    }
}

#[derive(Debug, PartialEq)]
pub enum TransformError {
    Json(ApplyToError),
    Jq(jaq_interpret::Error),
}

impl Display for TransformError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TransformError::Json(err) => write!(f, "{}", err.message().unwrap_or_default()),
            TransformError::Jq(err) => write!(f, "{}", err),
        }
    }
}

impl Display for Selection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Selection::Json { source, .. } => write!(f, "{}", source),
            Selection::Jq { source, .. } => write!(f, "{}", source),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Group<'a> {
    Json(&'a SubSelection),
    Jq(&'a Vec<KeyVal<Spanned<Filter>>>),
}

impl<'a> Group<'a> {
    pub(crate) fn fields(&self) -> Result<Vec<SelectionField<'a>>, &'static str> {
        match self {
            Self::Json(selection) => Ok(selection
                .selections_iter()
                .map(|selection| SelectionField {
                    name: selection.name(),
                    group: selection.next_subselection().map(Group::Json),
                })
                .collect()),
            Self::Jq(kevals) => kevals
                .iter()
                .map(|keyval| match keyval {
                    KeyVal::Str(field, filter) => {
                        if field.fmt.is_some() {
                            return Err("expected string");
                        }
                        match &field.parts[0] {
                            Part::Str(s) => Ok(SelectionField {
                                name: s.as_str(),
                                group: filter.as_ref().and_then(|spanned| jq_group(&spanned.0)),
                            }),
                            Part::Fun(_) => Err("expected string"),
                        }
                    }
                    KeyVal::Filter(key, filter) => match &key.0 {
                        Filter::Str(field) => match &field.parts[0] {
                            Part::Str(s) => Ok(SelectionField {
                                name: s.as_str(),
                                group: jq_group(&filter.0),
                            }),
                            Part::Fun(_) => Err("expected string"),
                        },
                        _ => Err("keys must be strings"),
                    },
                })
                .try_collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct SelectionField<'a> {
    pub(crate) name: &'a str,
    pub(crate) group: Option<Group<'a>>,
}
