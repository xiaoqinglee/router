use derivative::Derivative;
use opentelemetry_api::Value;
use schemars::JsonSchema;
use serde::Deserialize;
use tower::BoxError;

use crate::plugins::telemetry::config::AttributeValue;
use crate::plugins::telemetry::config_new::connector::ConnectorRequest;
use crate::plugins::telemetry::config_new::connector::ConnectorResponse;
use crate::plugins::telemetry::config_new::instruments::InstrumentValue;
use crate::plugins::telemetry::config_new::instruments::Standard;
use crate::plugins::telemetry::config_new::selectors::ErrorRepr;
use crate::plugins::telemetry::config_new::selectors::ResponseStatus;
use crate::plugins::telemetry::config_new::Selector;
use crate::plugins::telemetry::config_new::Stage;
use crate::services::connector_service::ConnectorInfo;
use crate::services::connector_service::CONNECTOR_INFO_CONTEXT_KEY;
use crate::Context;

#[derive(Deserialize, JsonSchema, Clone, Debug, PartialEq)]
#[serde(deny_unknown_fields, rename_all = "snake_case")]
pub(crate) enum ConnectorSource {
    /// The name of the connector source.
    Name,
}

#[derive(Deserialize, JsonSchema, Clone, Debug)]
#[serde(deny_unknown_fields, rename_all = "snake_case", untagged)]
pub(crate) enum ConnectorValue {
    Standard(Standard),
    Custom(ConnectorSelector),
}

impl From<&ConnectorValue> for InstrumentValue<ConnectorSelector> {
    fn from(value: &ConnectorValue) -> Self {
        match value {
            ConnectorValue::Standard(s) => InstrumentValue::Standard(s.clone()),
            ConnectorValue::Custom(selector) => InstrumentValue::Custom(selector.clone()),
        }
    }
}
#[derive(Deserialize, JsonSchema, Clone, Derivative)]
#[serde(deny_unknown_fields, rename_all = "snake_case", untagged)]
#[derivative(Debug, PartialEq)]
pub(crate) enum ConnectorSelector {
    SubgraphName {
        /// The subgraph name
        subgraph_name: bool,
    },
    ConnectorSource {
        /// The connector source.
        connector_source: ConnectorSource,
    },
    HttpRequestHeader {
        /// The name of a connector HTTP request header.
        connector_http_request_header: String,
        #[serde(skip)]
        #[allow(dead_code)]
        /// Optional redaction pattern.
        redact: Option<String>,
        /// Optional default value.
        default: Option<String>,
    },
    ConnectorResponseHeader {
        /// The name of a connector HTTP response header.
        connector_http_response_header: String,
        #[serde(skip)]
        #[allow(dead_code)]
        /// Optional redaction pattern.
        redact: Option<String>,
        /// Optional default value.
        default: Option<String>,
    },
    ConnectorResponseStatus {
        /// The connector HTTP response status code.
        connector_http_response_status: ResponseStatus,
    },
    ConnectorHttpMethod {
        /// The connector HTTP method.
        connector_http_method: bool,
    },
    ConnectorUrlTemplate {
        /// The connector URL template.
        connector_url_template: bool,
    },
    StaticField {
        /// A static value
        r#static: AttributeValue,
    },
    Error {
        /// Critical error if it happens
        error: ErrorRepr,
    },
}

impl Selector for ConnectorSelector {
    type Request = ConnectorRequest;
    type Response = ConnectorResponse;
    type EventResponse = ();

    fn on_request(&self, request: &Self::Request) -> Option<Value> {
        let connector_info = request
            .context
            .get::<&str, ConnectorInfo>(CONNECTOR_INFO_CONTEXT_KEY);
        match self {
            ConnectorSelector::SubgraphName { subgraph_name } if *subgraph_name => connector_info
                .ok()
                .flatten()
                .map(|info| info.subgraph_name.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorSource { .. } => connector_info
                .ok()
                .flatten()
                .and_then(|info| info.source_name.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorHttpMethod {
                connector_http_method,
            } if *connector_http_method => connector_info
                .ok()
                .flatten()
                .map(|info| info.http_method.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorUrlTemplate {
                connector_url_template,
            } if *connector_url_template => connector_info
                .ok()
                .flatten()
                .map(|info| info.url_template.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::HttpRequestHeader {
                connector_http_request_header: connector_request_header,
                default,
                ..
            } => request
                .http_request
                .headers()
                .get(connector_request_header)
                .and_then(|h| Some(h.to_str().ok()?.to_string()))
                .or_else(|| default.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::StaticField { r#static } => Some(r#static.clone().into()),
            _ => None,
        }
    }

    fn on_response(&self, response: &Self::Response) -> Option<Value> {
        let connector_info = response
            .context
            .get::<&str, ConnectorInfo>(CONNECTOR_INFO_CONTEXT_KEY);
        match self {
            ConnectorSelector::SubgraphName { subgraph_name } if *subgraph_name => connector_info
                .ok()
                .flatten()
                .map(|info| info.subgraph_name.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorSource { .. } => connector_info
                .ok()
                .flatten()
                .and_then(|info| info.source_name.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorHttpMethod {
                connector_http_method,
            } if *connector_http_method => connector_info
                .ok()
                .flatten()
                .map(|info| info.http_method.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorUrlTemplate {
                connector_url_template,
            } if *connector_url_template => connector_info
                .ok()
                .flatten()
                .map(|info| info.url_template.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorResponseHeader {
                connector_http_response_header: connector_response_header,
                default,
                ..
            } => response
                .http_response
                .headers()
                .get(connector_response_header)
                .and_then(|h| Some(h.to_str().ok()?.to_string()))
                .or_else(|| default.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorResponseStatus {
                connector_http_response_status: response_status,
            } => match response_status {
                ResponseStatus::Code => {
                    Some(Value::I64(response.http_response.status().as_u16() as i64))
                }
                ResponseStatus::Reason => response
                    .http_response
                    .status()
                    .canonical_reason()
                    .map(|reason| reason.into()),
            },
            ConnectorSelector::StaticField { r#static } => Some(r#static.clone().into()),
            _ => None,
        }
    }

    fn on_error(&self, error: &BoxError, ctx: &Context) -> Option<Value> {
        let connector_info = ctx.get::<&str, ConnectorInfo>(CONNECTOR_INFO_CONTEXT_KEY);
        match self {
            ConnectorSelector::SubgraphName { subgraph_name } if *subgraph_name => connector_info
                .ok()
                .flatten()
                .map(|info| info.subgraph_name.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorSource { .. } => connector_info
                .ok()
                .flatten()
                .and_then(|info| info.source_name.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorHttpMethod {
                connector_http_method,
            } if *connector_http_method => connector_info
                .ok()
                .flatten()
                .map(|info| info.http_method.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::ConnectorUrlTemplate {
                connector_url_template,
            } if *connector_url_template => connector_info
                .ok()
                .flatten()
                .map(|info| info.url_template.clone())
                .map(opentelemetry::Value::from),
            ConnectorSelector::Error { .. } => Some(error.to_string().into()),
            ConnectorSelector::StaticField { r#static } => Some(r#static.clone().into()),
            _ => None,
        }
    }

    fn on_drop(&self) -> Option<Value> {
        match self {
            ConnectorSelector::StaticField { r#static } => Some(r#static.clone().into()),
            _ => None,
        }
    }

    fn is_active(&self, stage: Stage) -> bool {
        match stage {
            Stage::Request => matches!(
                self,
                ConnectorSelector::HttpRequestHeader { .. }
                    | ConnectorSelector::SubgraphName { .. }
                    | ConnectorSelector::ConnectorSource { .. }
                    | ConnectorSelector::ConnectorHttpMethod { .. }
                    | ConnectorSelector::ConnectorUrlTemplate { .. }
                    | ConnectorSelector::StaticField { .. }
            ),
            Stage::Response => matches!(
                self,
                ConnectorSelector::ConnectorResponseHeader { .. }
                    | ConnectorSelector::ConnectorResponseStatus { .. }
                    | ConnectorSelector::SubgraphName { .. }
                    | ConnectorSelector::ConnectorSource { .. }
                    | ConnectorSelector::ConnectorHttpMethod { .. }
                    | ConnectorSelector::ConnectorUrlTemplate { .. }
                    | ConnectorSelector::StaticField { .. }
            ),
            Stage::ResponseEvent => false,
            Stage::ResponseField => false,
            Stage::Error => matches!(
                self,
                ConnectorSelector::Error { .. }
                    | ConnectorSelector::SubgraphName { .. }
                    | ConnectorSelector::ConnectorSource { .. }
                    | ConnectorSelector::ConnectorHttpMethod { .. }
                    | ConnectorSelector::ConnectorUrlTemplate { .. }
                    | ConnectorSelector::StaticField { .. }
            ),
            Stage::Drop => matches!(self, ConnectorSelector::StaticField { .. }),
        }
    }
}

#[cfg(test)]
mod tests {
    use apollo_federation::sources::connect::HTTPMethod;
    use http::StatusCode;

    use super::ConnectorSelector;
    use super::ConnectorSource;
    use crate::plugins::telemetry::config_new::connector::ConnectorRequest;
    use crate::plugins::telemetry::config_new::connector::ConnectorResponse;
    use crate::plugins::telemetry::config_new::selectors::ResponseStatus;
    use crate::plugins::telemetry::config_new::Selector;
    use crate::services::connector_service::ConnectorInfo;
    use crate::services::connector_service::CONNECTOR_INFO_CONTEXT_KEY;
    use crate::services::http::HttpRequest;
    use crate::services::http::HttpResponse;
    use crate::Context;

    const TEST_SUBGRAPH_NAME: &str = "test_subgraph_name";
    const TEST_SOURCE_NAME: &str = "test_source_name";
    const TEST_URL_TEMPLATE: &str = "/test";
    const TEST_HEADER_NAME: &str = "test_header_name";
    const TEST_HEADER_VALUE: &str = "test_header_value";
    const TEST_STATIC: &str = "test_static";

    fn connector_info() -> ConnectorInfo {
        ConnectorInfo {
            subgraph_name: TEST_SUBGRAPH_NAME.to_string(),
            source_name: Some(TEST_SOURCE_NAME.to_string()),
            http_method: HTTPMethod::Get.as_str().to_string(),
            url_template: TEST_URL_TEMPLATE.to_string(),
        }
    }

    fn context(connector_info: ConnectorInfo) -> Context {
        let context = Context::default();
        context
            .insert(CONNECTOR_INFO_CONTEXT_KEY, connector_info)
            .unwrap();
        context
    }

    fn http_request(context: Context) -> ConnectorRequest {
        HttpRequest {
            http_request: http::Request::builder().body("".into()).unwrap(),
            context,
        }
    }

    fn http_request_with_header(context: Context) -> ConnectorRequest {
        HttpRequest {
            http_request: http::Request::builder()
                .header(TEST_HEADER_NAME, TEST_HEADER_VALUE)
                .body("".into())
                .unwrap(),
            context,
        }
    }

    fn http_response(context: Context, status_code: StatusCode) -> ConnectorResponse {
        HttpResponse {
            http_response: http::Response::builder()
                .status(status_code)
                .body("".into())
                .unwrap(),
            context,
        }
    }

    fn http_response_with_header(context: Context, status_code: StatusCode) -> ConnectorResponse {
        HttpResponse {
            http_response: http::Response::builder()
                .status(status_code)
                .header(TEST_HEADER_NAME, TEST_HEADER_VALUE)
                .body("".into())
                .unwrap(),
            context,
        }
    }

    #[test]
    fn connector_on_request_static_field() {
        let selector = ConnectorSelector::StaticField {
            r#static: TEST_STATIC.into(),
        };
        assert_eq!(
            Some(TEST_STATIC.into()),
            selector.on_request(&http_request(context(connector_info())))
        );
    }

    #[test]
    fn connector_on_request_subgraph_name() {
        let selector = ConnectorSelector::SubgraphName {
            subgraph_name: true,
        };
        assert_eq!(
            Some(TEST_SUBGRAPH_NAME.into()),
            selector.on_request(&http_request(context(connector_info())))
        );
    }

    #[test]
    fn connector_on_request_connector_source() {
        let selector = ConnectorSelector::ConnectorSource {
            connector_source: ConnectorSource::Name,
        };
        assert_eq!(
            Some(TEST_SOURCE_NAME.into()),
            selector.on_request(&http_request(context(connector_info())))
        );
    }

    #[test]
    fn connector_on_request_url_template() {
        let selector = ConnectorSelector::ConnectorUrlTemplate {
            connector_url_template: true,
        };
        assert_eq!(
            Some(TEST_URL_TEMPLATE.into()),
            selector.on_request(&http_request(context(connector_info())))
        );
    }

    #[test]
    fn connector_on_request_header_defaulted() {
        let selector = ConnectorSelector::HttpRequestHeader {
            connector_http_request_header: TEST_HEADER_NAME.to_string(),
            redact: None,
            default: Some("defaulted".into()),
        };
        assert_eq!(
            Some("defaulted".into()),
            selector.on_request(&http_request(context(connector_info())))
        );
    }

    #[test]
    fn connector_on_request_header_with_value() {
        let selector = ConnectorSelector::HttpRequestHeader {
            connector_http_request_header: TEST_HEADER_NAME.to_string(),
            redact: None,
            default: None,
        };
        assert_eq!(
            Some(TEST_HEADER_VALUE.into()),
            selector.on_request(&http_request_with_header(context(connector_info())))
        );
    }

    #[test]
    fn connector_on_response_static_field() {
        let selector = ConnectorSelector::StaticField {
            r#static: TEST_STATIC.into(),
        };
        assert_eq!(
            Some(TEST_STATIC.into()),
            selector.on_response(&http_response(context(connector_info()), StatusCode::OK))
        );
    }

    #[test]
    fn connector_on_response_subgraph_name() {
        let selector = ConnectorSelector::SubgraphName {
            subgraph_name: true,
        };
        assert_eq!(
            Some(TEST_SUBGRAPH_NAME.into()),
            selector.on_response(&http_response(context(connector_info()), StatusCode::OK))
        );
    }

    #[test]
    fn connector_on_response_connector_source() {
        let selector = ConnectorSelector::ConnectorSource {
            connector_source: ConnectorSource::Name,
        };
        assert_eq!(
            Some(TEST_SOURCE_NAME.into()),
            selector.on_response(&http_response(context(connector_info()), StatusCode::OK))
        );
    }

    #[test]
    fn connector_on_response_url_template() {
        let selector = ConnectorSelector::ConnectorUrlTemplate {
            connector_url_template: true,
        };
        assert_eq!(
            Some(TEST_URL_TEMPLATE.into()),
            selector.on_response(&http_response(context(connector_info()), StatusCode::OK))
        );
    }

    #[test]
    fn connector_on_response_header_defaulted() {
        let selector = ConnectorSelector::ConnectorResponseHeader {
            connector_http_response_header: TEST_HEADER_NAME.to_string(),
            redact: None,
            default: Some("defaulted".into()),
        };
        assert_eq!(
            Some("defaulted".into()),
            selector.on_response(&http_response(context(connector_info()), StatusCode::OK))
        );
    }

    #[test]
    fn connector_on_response_header_with_value() {
        let selector = ConnectorSelector::ConnectorResponseHeader {
            connector_http_response_header: TEST_HEADER_NAME.to_string(),
            redact: None,
            default: None,
        };
        assert_eq!(
            Some(TEST_HEADER_VALUE.into()),
            selector.on_response(&http_response_with_header(
                context(connector_info()),
                StatusCode::OK
            ))
        );
    }

    #[test]
    fn connector_on_response_status_code() {
        let selector = ConnectorSelector::ConnectorResponseStatus {
            connector_http_response_status: ResponseStatus::Code,
        };
        assert_eq!(
            Some(200.into()),
            selector.on_response(&http_response(context(connector_info()), StatusCode::OK))
        );
    }

    #[test]
    fn connector_on_response_status_reason_ok() {
        let selector = ConnectorSelector::ConnectorResponseStatus {
            connector_http_response_status: ResponseStatus::Reason,
        };
        assert_eq!(
            Some("OK".into()),
            selector.on_response(&http_response(context(connector_info()), StatusCode::OK))
        );
    }

    #[test]
    fn connector_on_response_status_code_not_found() {
        let selector = ConnectorSelector::ConnectorResponseStatus {
            connector_http_response_status: ResponseStatus::Reason,
        };
        assert_eq!(
            Some("Not Found".into()),
            selector.on_response(&http_response(
                context(connector_info()),
                StatusCode::NOT_FOUND
            ))
        );
    }

    #[test]
    fn connector_on_response_http_method() {
        let selector = ConnectorSelector::ConnectorHttpMethod {
            connector_http_method: true,
        };
        assert_eq!(
            Some(HTTPMethod::Get.as_str().into()),
            selector.on_response(&http_response(context(connector_info()), StatusCode::OK))
        );
    }

    #[test]
    fn connector_on_drop_static_field() {
        let selector = ConnectorSelector::StaticField {
            r#static: TEST_STATIC.into(),
        };
        assert_eq!(Some(TEST_STATIC.into()), selector.on_drop());
    }
}
