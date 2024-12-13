//! Headers defined in connectors `@source` and `@connect` directives.

use std::ops::Deref;
use std::str::FromStr;

use apollo_compiler::collections::IndexMap;
use serde_json_bytes::Value;

use crate::sources::connect::string_template;
use crate::sources::connect::string_template::StringTemplate;

#[derive(Clone, Debug)]
pub struct HeaderValue(StringTemplate<http::HeaderValue>);

impl HeaderValue {
    /// Evaluate expressions in the header value.
    ///
    /// # Errors
    ///
    /// Returns an error any expression can't be evaluated, or evaluates to an unsupported type.
    pub fn interpolate(&self, vars: &IndexMap<String, Value>) -> Result<http::HeaderValue, String> {
        let mut result = Vec::new();
        for part in &self.parts {
            let value = part.interpolate(vars).map_err(|err| err.message)?;
            result.extend(value.as_bytes());
        }
        http::HeaderValue::from_bytes(&result).map_err(|e| e.to_string())
    }
}

impl Deref for HeaderValue {
    type Target = StringTemplate<http::HeaderValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl FromStr for HeaderValue {
    type Err = string_template::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        StringTemplate::parse(s, 0).map(HeaderValue)
    }
}

#[cfg(test)]
mod test_header_value_parse {
    use insta::assert_debug_snapshot;

    use super::*;

    #[test]
    fn simple_constant() {
        assert_debug_snapshot!(
            HeaderValue::from_str("text"),
            @r###"
        Ok(
            HeaderValue(
                StringTemplate {
                    parts: [
                        Constant(
                            Constant {
                                value: "text",
                                location: 0..4,
                            },
                        ),
                    ],
                },
            ),
        )
        "###
        );
    }
    #[test]
    fn simple_expression() {
        assert_debug_snapshot!(
            HeaderValue::from_str("{$config.one}"),
            @r###"
        Ok(
            HeaderValue(
                StringTemplate {
                    parts: [
                        Expression(
                            Expression {
                                expression: Path(
                                    PathSelection {
                                        path: WithRange {
                                            node: Var(
                                                WithRange {
                                                    node: $config,
                                                    range: Some(
                                                        0..7,
                                                    ),
                                                },
                                                WithRange {
                                                    node: Key(
                                                        WithRange {
                                                            node: Field(
                                                                "one",
                                                            ),
                                                            range: Some(
                                                                8..11,
                                                            ),
                                                        },
                                                        WithRange {
                                                            node: Empty,
                                                            range: Some(
                                                                11..11,
                                                            ),
                                                        },
                                                    ),
                                                    range: Some(
                                                        7..11,
                                                    ),
                                                },
                                            ),
                                            range: Some(
                                                0..11,
                                            ),
                                        },
                                    },
                                ),
                                location: 1..12,
                            },
                        ),
                    ],
                },
            ),
        )
        "###
        );
    }
    #[test]
    fn mixed_constant_and_expression() {
        assert_debug_snapshot!(
            HeaderValue::from_str("text{$config.one}text"),
            @r###"
        Ok(
            HeaderValue(
                StringTemplate {
                    parts: [
                        Constant(
                            Constant {
                                value: "text",
                                location: 0..4,
                            },
                        ),
                        Expression(
                            Expression {
                                expression: Path(
                                    PathSelection {
                                        path: WithRange {
                                            node: Var(
                                                WithRange {
                                                    node: $config,
                                                    range: Some(
                                                        0..7,
                                                    ),
                                                },
                                                WithRange {
                                                    node: Key(
                                                        WithRange {
                                                            node: Field(
                                                                "one",
                                                            ),
                                                            range: Some(
                                                                8..11,
                                                            ),
                                                        },
                                                        WithRange {
                                                            node: Empty,
                                                            range: Some(
                                                                11..11,
                                                            ),
                                                        },
                                                    ),
                                                    range: Some(
                                                        7..11,
                                                    ),
                                                },
                                            ),
                                            range: Some(
                                                0..11,
                                            ),
                                        },
                                    },
                                ),
                                location: 5..16,
                            },
                        ),
                        Constant(
                            Constant {
                                value: "text",
                                location: 17..21,
                            },
                        ),
                    ],
                },
            ),
        )
        "###
        );
    }

    #[test]
    fn expressions_with_nested_braces() {
        assert_debug_snapshot!(
            HeaderValue::from_str("const{$config.one { two { three } }}another-const"),
            @r###"
        Ok(
            HeaderValue(
                StringTemplate {
                    parts: [
                        Constant(
                            Constant {
                                value: "const",
                                location: 0..5,
                            },
                        ),
                        Expression(
                            Expression {
                                expression: Path(
                                    PathSelection {
                                        path: WithRange {
                                            node: Var(
                                                WithRange {
                                                    node: $config,
                                                    range: Some(
                                                        0..7,
                                                    ),
                                                },
                                                WithRange {
                                                    node: Key(
                                                        WithRange {
                                                            node: Field(
                                                                "one",
                                                            ),
                                                            range: Some(
                                                                8..11,
                                                            ),
                                                        },
                                                        WithRange {
                                                            node: Selection(
                                                                SubSelection {
                                                                    selections: [
                                                                        Field(
                                                                            None,
                                                                            WithRange {
                                                                                node: Field(
                                                                                    "two",
                                                                                ),
                                                                                range: Some(
                                                                                    14..17,
                                                                                ),
                                                                            },
                                                                            Some(
                                                                                SubSelection {
                                                                                    selections: [
                                                                                        Field(
                                                                                            None,
                                                                                            WithRange {
                                                                                                node: Field(
                                                                                                    "three",
                                                                                                ),
                                                                                                range: Some(
                                                                                                    20..25,
                                                                                                ),
                                                                                            },
                                                                                            None,
                                                                                        ),
                                                                                    ],
                                                                                    range: Some(
                                                                                        18..27,
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        ),
                                                                    ],
                                                                    range: Some(
                                                                        12..29,
                                                                    ),
                                                                },
                                                            ),
                                                            range: Some(
                                                                12..29,
                                                            ),
                                                        },
                                                    ),
                                                    range: Some(
                                                        7..29,
                                                    ),
                                                },
                                            ),
                                            range: Some(
                                                0..29,
                                            ),
                                        },
                                    },
                                ),
                                location: 6..35,
                            },
                        ),
                        Constant(
                            Constant {
                                value: "another-const",
                                location: 36..49,
                            },
                        ),
                    ],
                },
            ),
        )
        "###
        );
    }

    #[test]
    fn invalid_header_values() {
        assert_debug_snapshot!(
            HeaderValue::from_str("\n"),
            @r###"
        Err(
            Error {
                message: "invalid value `\n`",
                location: 0..1,
            },
        )
        "###
        )
    }

    #[test]
    fn missing_closing_braces() {
        assert_debug_snapshot!(
            HeaderValue::from_str("{$config.one"),
            @r###"
        Err(
            Error {
                message: "Invalid expression, missing closing }",
                location: 0..12,
            },
        )
        "###
        )
    }
}

#[cfg(test)]
mod test_interpolate {
    use insta::assert_debug_snapshot;
    use pretty_assertions::assert_eq;
    use serde_json_bytes::json;

    use super::*;
    #[test]
    fn test_interpolate() {
        let value = HeaderValue::from_str("before {$config.one} after").unwrap();
        let mut vars = IndexMap::default();
        vars.insert("$config".to_string(), json!({"one": "foo"}));
        assert_eq!(
            value.interpolate(&vars),
            Ok(http::HeaderValue::from_static("before foo after"))
        );
    }

    #[test]
    fn test_interpolate_missing_value() {
        let value = HeaderValue::from_str("{$config.one}").unwrap();
        let vars = IndexMap::default();
        assert_eq!(
            value.interpolate(&vars),
            Ok(http::HeaderValue::from_static(""))
        );
    }

    #[test]
    fn test_interpolate_value_array() {
        let header_value = HeaderValue::from_str("{$config.one}").unwrap();
        let mut vars = IndexMap::default();
        vars.insert("$config".to_string(), json!({"one": ["one", "two"]}));
        assert_eq!(
            header_value.interpolate(&vars),
            Err("Header expressions can't evaluate to arrays or objects.".to_string())
        );
    }

    #[test]
    fn test_interpolate_value_bool() {
        let header_value = HeaderValue::from_str("{$config.one}").unwrap();
        let mut vars = IndexMap::default();
        vars.insert("$config".to_string(), json!({"one": true}));
        assert_eq!(
            Ok(http::HeaderValue::from_static("true")),
            header_value.interpolate(&vars)
        );
    }

    #[test]
    fn test_interpolate_value_null() {
        let header_value = HeaderValue::from_str("{$config.one}").unwrap();
        let mut vars = IndexMap::default();
        vars.insert("$config".to_string(), json!({"one": null}));
        assert_eq!(
            Ok(http::HeaderValue::from_static("")),
            header_value.interpolate(&vars)
        );
    }

    #[test]
    fn test_interpolate_value_number() {
        let header_value = HeaderValue::from_str("{$config.one}").unwrap();
        let mut vars = IndexMap::default();
        vars.insert("$config".to_string(), json!({"one": 1}));
        assert_eq!(
            Ok(http::HeaderValue::from_static("1")),
            header_value.interpolate(&vars)
        );
    }

    #[test]
    fn test_interpolate_value_object() {
        let header_value = HeaderValue::from_str("{$config.one}").unwrap();
        let mut vars = IndexMap::default();
        vars.insert("$config".to_string(), json!({"one": {}}));
        assert_debug_snapshot!(
            header_value.interpolate(&vars),
            @r###"
        Err(
            "Header expressions can't evaluate to arrays or objects.",
        )
        "###
        );
    }

    #[test]
    fn test_interpolate_value_string() {
        let header_value = HeaderValue::from_str("{$config.one}").unwrap();
        let mut vars = IndexMap::default();
        vars.insert("$config".to_string(), json!({"one": "string"}));
        assert_eq!(
            Ok(http::HeaderValue::from_static("string")),
            header_value.interpolate(&vars)
        );
    }
}

#[cfg(test)]
mod test_get_expressions {
    use super::*;

    #[test]
    fn test_variable_references() {
        let value =
            HeaderValue::from_str("a {$this.a.b.c} b {$args.a.b.c} c {$config.a.b.c}").unwrap();
        let references: Vec<_> = value
            .expressions()
            .map(|e| e.expression.to_string())
            .collect();
        assert_eq!(
            references,
            vec!["$this.a.b.c", "$args.a.b.c", "$config.a.b.c"]
        );
    }
}
