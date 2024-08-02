use std::fmt::Display;
use std::str::FromStr;
use std::sync::Arc;

use apollo_compiler::collections::IndexMap;
use apollo_compiler::executable::SelectionSet;
use itertools::Itertools;
use jaq_interpret::Ctx;
use jaq_interpret::FilterT;
use jaq_interpret::ParseCtx;
use jaq_interpret::RcIter;
use jaq_interpret::Val;
use jaq_syn::filter::Filter;
use jaq_syn::filter::KeyVal;
use jaq_syn::string::Part;
use jaq_syn::Lexer;
use jaq_syn::Main;
use jaq_syn::Parser;
use jaq_syn::Spanned;
pub use json_selection::JSONSelection;
pub use json_selection::JSONSelectionVisitor; // TODO: use the new visitors for validation
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
            Selection::Jq { parsed, .. } => match &parsed.body.0 {
                Filter::Object(keyvals) => Some(Group::Jq(keyvals)),
                other => {
                    let err = format!("Expected object filter, got: {:?}", other);
                    None
                }
            },
        }
    }

    pub fn parse_json_selection(source: &str) -> Result<Self, String> {
        let (remainder, parsed) = JSONSelection::parse(&source).map_err(|err| err.to_string())?;
        if remainder != "" {
            return Err(format!(
                "JSONSelection parsing failed, trailing characters: {}",
                remainder
            ));
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
            Selection::Jq { parsed, .. } => {
                // TODO: make execution happen in router, dep not needed in apollo-federation
                let mut defs = ParseCtx::new(Vec::new());
                // TODO: only compile once per connector, not every request
                let f = defs.compile(parsed.clone());
                // TODO: stop needing to convert to/from serde_json_bytes
                let json = serde_json::Value::from_str(&json.to_string()).unwrap();
                // TODO: pass through vars here
                let inputs = RcIter::new(core::iter::empty());
                let out = f.run((Ctx::new([], &inputs), Val::from(json)));
                let mut errs = Vec::new();
                for res in out {
                    match res {
                        Err(err) => errs.push(TransformError::Jq(err)),
                        Ok(val) => {
                            return (
                                Some(serde_json_bytes::Value::from(serde_json::Value::from(val))),
                                errs,
                            );
                        }
                    }
                }
                return (None, errs);
            }
        }
    }
}

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
                                group: filter.as_ref().and_then(|(filter, _span)| match filter {
                                    Filter::Object(keyvals) => Some(Group::Jq(keyvals)),
                                    _ => None, // TODO: handle arrays of objects too!
                                }),
                            }),
                            Part::Fun(_) => Err("expected string"),
                        }
                    }
                    KeyVal::Filter(key, filter) => match &key.0 {
                        Filter::Str(field) => match &field.parts[0] {
                            Part::Str(s) => Ok(SelectionField {
                                name: s.as_str(),
                                group: match &filter.0 {
                                    Filter::Object(keyvals) => Some(Group::Jq(keyvals)),
                                    _ => None, // TODO: handle arrays of objects too!
                                },
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
