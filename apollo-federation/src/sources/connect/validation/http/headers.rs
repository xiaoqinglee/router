use std::collections::HashMap;

use apollo_compiler::ast::Value;
use apollo_compiler::Name;
use apollo_compiler::Node;

use crate::sources::connect::header::HeaderValue;
use crate::sources::connect::models::Header;
use crate::sources::connect::models::HeaderParseError;
use crate::sources::connect::spec::schema::HEADERS_ARGUMENT_NAME;
use crate::sources::connect::string_template;
use crate::sources::connect::string_template::Expression;
use crate::sources::connect::validation::coordinates::HttpHeadersCoordinate;
use crate::sources::connect::validation::graphql::GraphQLString;
use crate::sources::connect::validation::graphql::SchemaInfo;
use crate::sources::connect::validation::selection::validate_selection_variables;
use crate::sources::connect::validation::variable::VariableResolver;
use crate::sources::connect::validation::Code;
use crate::sources::connect::validation::Message;
use crate::sources::connect::variable::ConnectorsContext;
use crate::sources::connect::variable::Directive;
use crate::sources::connect::variable::Phase;
use crate::sources::connect::variable::Target;
use crate::sources::connect::HeaderSource;

pub(crate) fn validate_arg<'a>(
    http_arg: &'a [(Name, Node<Value>)],
    schema: &'a SchemaInfo,
    coordinate: HttpHeadersCoordinate<'a>,
) -> Vec<Message> {
    let mut messages = Vec::new();
    let Some(headers_arg) = get_arg(http_arg) else {
        return messages;
    };

    let expression_context = match coordinate {
        HttpHeadersCoordinate::Source { .. } => {
            ConnectorsContext::new(Directive::Source, Phase::Request, Target::Header)
        }
        HttpHeadersCoordinate::Connect { connect, .. } => {
            ConnectorsContext::new(connect.into(), Phase::Request, Target::Header)
        }
    };
    let variable_resolver = VariableResolver::new(expression_context.clone(), schema);

    #[allow(clippy::mutable_key_type)]
    let mut names = HashMap::new();
    for header in Header::from_headers_arg(headers_arg) {
        let Header {
            name,
            name_node,
            source,
            source_node,
        } = match header {
            Ok(header) => header,
            Err(err) => {
                let (message, locations) = match err {
                    HeaderParseError::Other { message, node } => (
                        message,
                        node.line_column_range(&schema.sources)
                            .into_iter()
                            .collect(),
                    ),
                    HeaderParseError::ConflictingArguments {
                        message,
                        from_location,
                        value_location,
                    } => (
                        message,
                        from_location
                            .iter()
                            .chain(value_location.iter())
                            .flat_map(|span| span.line_column_range(&schema.sources))
                            .collect(),
                    ),
                    HeaderParseError::ValueError {
                        err: string_template::Error { message, location },
                        node,
                    } => (
                        message,
                        GraphQLString::new(node, &schema.sources)
                            .ok()
                            .and_then(|expression| {
                                expression.line_col_for_subslice(location, schema)
                            })
                            .into_iter()
                            .collect(),
                    ),
                };
                messages.push(Message {
                    code: Code::InvalidHeader,
                    message: format!("In {coordinate} {message}"),
                    locations,
                });
                continue;
            }
        };
        if let Some(duplicate) = names.insert(name.clone(), name_node.location()) {
            messages.push(Message {
                code: Code::HttpHeaderNameCollision,
                message: format!(
                    "Duplicate header names are not allowed. The header name '{name}' at {coordinate} is already defined.",
                ),
                locations: name_node.line_column_range(&schema.sources)
                    .into_iter()
                    .chain(
                        duplicate.and_then(|span| span.line_column_range(&schema.sources))
                    )
                    .collect(),
            });
            continue;
        }
        if let HeaderSource::Value(header_value) = source {
            let Ok(expression) = GraphQLString::new(source_node, &schema.sources) else {
                // This should never fail in practice, we convert to GraphQLString only to hack in location data
                continue;
            };
            messages.extend(
                validate_value(
                    header_value,
                    expression,
                    &variable_resolver,
                    schema,
                    coordinate,
                )
                .err(),
            );
        }
    }
    messages
}

fn validate_value(
    header_value: HeaderValue,
    source_string: GraphQLString,
    variable_resolver: &VariableResolver,
    _schema: &SchemaInfo, // TODO: type checking with Shape
    coordinate: HttpHeadersCoordinate,
) -> Result<(), Message> {
    for Expression {
        expression,
        location,
    } in header_value.expressions()
    {
        validate_selection_variables(
            variable_resolver,
            coordinate,
            expression,
            source_string.slice(location.clone()),
        )?;
    }

    Ok(())
}

fn get_arg(http_arg: &[(Name, Node<Value>)]) -> Option<&Node<Value>> {
    http_arg
        .iter()
        .find_map(|(key, value)| (*key == HEADERS_ARGUMENT_NAME).then_some(value))
}
