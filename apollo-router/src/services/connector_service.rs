//! Tower service for connectors.

use std::collections::HashMap;
use std::str::FromStr;
use std::sync::Arc;
use std::task::Poll;

use apollo_compiler::ast::Value;
use apollo_compiler::validation::Valid;
use apollo_federation::sources::connect::Connector;
use axum::body::HttpBody;
use extism::Manifest;
use extism::Plugin;
use extism::Wasm;
use futures::future::BoxFuture;
use indexmap::IndexMap;
use opentelemetry::Key;
use parking_lot::Mutex;
use serde_json_bytes::json;
use tower::BoxError;
use tower::ServiceExt;
use tracing::Instrument;

use super::connect::BoxService;
use super::http::HttpClientServiceFactory;
use super::http::HttpRequest;
use super::new_service::ServiceFactory;
use crate::error::FetchError;
use crate::graphql;
use crate::plugins::connectors::error::Error as ConnectorError;
use crate::plugins::connectors::error::Error::WasmCallFailed;
use crate::plugins::connectors::handle_responses::handle_responses;
use crate::plugins::connectors::http::Request;
use crate::plugins::connectors::http::Response as ConnectorResponse;
use crate::plugins::connectors::http::Result as ConnectorResult;
use crate::plugins::connectors::make_requests::make_requests;
use crate::plugins::connectors::plugin::ConnectorContext;
use crate::plugins::connectors::request_limit::RequestLimit;
use crate::plugins::connectors::request_limit::RequestLimits;
use crate::plugins::connectors::tracing::CONNECTOR_TYPE_HTTP;
use crate::plugins::connectors::tracing::CONNECT_SPAN_NAME;
use crate::plugins::subscription::SubscriptionConfig;
use crate::services::router::body::RouterBody;
use crate::services::ConnectRequest;
use crate::services::ConnectResponse;
use crate::spec::Schema;
use crate::Context;

pub(crate) const APOLLO_CONNECTOR_TYPE: Key = Key::from_static_str("apollo.connector.type");
pub(crate) const APOLLO_CONNECTOR_DETAIL: Key = Key::from_static_str("apollo.connector.detail");
pub(crate) const APOLLO_CONNECTOR_SELECTION: Key =
    Key::from_static_str("apollo.connector.selection");
pub(crate) const APOLLO_CONNECTOR_FIELD_NAME: Key =
    Key::from_static_str("apollo.connector.field.name");
pub(crate) const APOLLO_CONNECTOR_FIELD_ALIAS: Key =
    Key::from_static_str("apollo.connector.field.alias");
pub(crate) const APOLLO_CONNECTOR_FIELD_RETURN_TYPE: Key =
    Key::from_static_str("apollo.connector.field.return_type");
pub(crate) const APOLLO_CONNECTOR_SOURCE_NAME: Key =
    Key::from_static_str("apollo.connector.source.name");
pub(crate) const APOLLO_CONNECTOR_SOURCE_DETAIL: Key =
    Key::from_static_str("apollo.connector.source.detail");

/// A service for executing connector requests.
#[derive(Clone)]
pub(crate) struct ConnectorService {
    pub(crate) http_service_factory: Arc<IndexMap<String, HttpClientServiceFactory>>,
    pub(crate) _schema: Arc<Schema>,
    pub(crate) _subgraph_schemas: Arc<HashMap<String, Arc<Valid<apollo_compiler::Schema>>>>,
    pub(crate) _subscription_config: Option<SubscriptionConfig>,
    pub(crate) connectors_by_service_name: Arc<IndexMap<Arc<str>, Connector>>,
}

impl tower::Service<ConnectRequest> for ConnectorService {
    type Response = ConnectResponse;
    type Error = BoxError;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, _cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, request: ConnectRequest) -> Self::Future {
        let connector = self
            .connectors_by_service_name
            .get(&request.service_name)
            .cloned();

        let http_client_factory = self
            .http_service_factory
            .get(&request.service_name.to_string())
            .cloned();

        Box::pin(async move {
            let Some(connector) = connector else {
                return Err("no connector found".into());
            };

            let Some(http_client_factory) = http_client_factory else {
                return Err("no http client found".into());
            };

            let fetch_time_offset = request.context.created_at.elapsed().as_nanos() as i64;
            let span = tracing::info_span!(
                CONNECT_SPAN_NAME,
                "otel.kind" = "INTERNAL",
                "apollo.connector.type" = CONNECTOR_TYPE_HTTP,
                "apollo.connector.detail" = tracing::field::Empty,
                "apollo.connector.field.name" = %connector.field_name(),
                "apollo.connector.selection" = %connector.selection,
                "apollo.connector.source.name" = tracing::field::Empty,
                "apollo.connector.source.detail" = tracing::field::Empty,
                "apollo_private.sent_time_offset" = fetch_time_offset,
            );
            // TODO: apollo.connector.field.alias
            // TODO: apollo.connector.field.return_type
            // TODO: apollo.connector.field.selection_set
            let transport = &connector.transport;
            if let Ok(detail) = serde_json::to_string(
                &serde_json::json!({ transport.method.as_str(): transport.connect_template.to_string() }),
            ) {
                span.record("apollo.connector.detail", detail);
            }
            if let Some(source_name) = connector.id.source_name.as_ref() {
                span.record("apollo.connector.source.name", source_name);
                if let Ok(detail) =
                    serde_json::to_string(&serde_json::json!({ "baseURL": transport.source_url }))
                {
                    span.record("apollo.connector.source.detail", detail);
                }
            }

            execute(&http_client_factory, request, &connector)
                .instrument(span)
                .await
        })
    }
}

async fn execute(
    http_client_factory: &HttpClientServiceFactory,
    request: ConnectRequest,
    connector: &Connector,
) -> Result<ConnectResponse, BoxError> {
    let context = request.context.clone();
    let original_subgraph_name = connector.id.subgraph_name.to_string();

    let (debug, request_limit) = context.extensions().with_lock(|lock| {
        let debug = lock.get::<Arc<Mutex<ConnectorContext>>>().cloned();
        let request_limit = lock
            .get::<Arc<RequestLimits>>()
            .map(|limits| limits.get((&connector.id).into(), connector.max_requests))
            .unwrap_or(None);
        (debug, request_limit)
    });

    let requests = make_requests(request, connector, &debug).map_err(BoxError::from)?;

    let tasks = requests.into_iter().map(|request| {
        run_request(
            request,
            context.clone(),
            original_subgraph_name.clone(),
            request_limit.clone(),
            http_client_factory,
            connector.id.label.clone(),
        )
    });

    let responses = futures::future::try_join_all(tasks)
        .await
        .map_err(BoxError::from)?;

    handle_responses(responses, connector, &debug)
        .await
        .map_err(BoxError::from)
}

async fn run_request(
    Request {
        request,
        key,
        debug_request,
    }: Request,
    context: Context,
    original_subgraph_name: String,
    request_limit: Option<Arc<RequestLimit>>,
    http_client_factory: &HttpClientServiceFactory,
    service: String,
) -> Result<ConnectorResponse<RouterBody>, BoxError> {
    let wasm_path = request.uri().authority().and_then(|authority| {
        let str_value = authority.as_str();
        str_value.ends_with(".wasm").then_some(str_value)
    });
    if let Some(wasm_path) = wasm_path {
        let wasm = Wasm::file(wasm_path);
        let manifest = Manifest::new([wasm]);
        let mut plugin = Plugin::new(&manifest, [], true)?;
        // TODO add context and stuff
        let inputs = key.inputs().merge(None, None);
        let input_str = serde_json::to_string(&inputs)?;
        return tokio::task::spawn_blocking(move || {
            // TODO: don't panic
            // TODO: Implement FromBytes for serde_json_bytes::Value
            let function_name = request.uri().path().trim_matches('/');
            let result = plugin
                .call::<&str, &str>(function_name, &input_str)
                .map_err(|err| WasmCallFailed(err.into()))
                .and_then(|result| {
                    serde_json_bytes::Value::from_str(&result)
                        .map_err(|err| WasmCallFailed(err.into()))
                })
                .into();
            return ConnectorResponse {
                result,
                key,
                debug_request,
            };
        })
        .await
        .map_err(|e| BoxError::from(e));
    }
    // Returning an error from this closure causes all tasks to be cancelled and the operation
    // to fail. This is the reason for the Result-wrapped-in-a-Result here. An `Err` on the
    // inner result fails just that one task, but an `Err` on the outer result cancels all the
    // tasks and fails the whole operation.
    let request_limit = request_limit.clone();
    if let Some(request_limit) = request_limit {
        if !request_limit.allow() {
            return Ok(ConnectorResponse {
                result: ConnectorResult::Err(ConnectorError::RequestLimitExceeded),
                key,
                debug_request,
            });
        }
    }
    let client = http_client_factory.create(&original_subgraph_name);
    let req = HttpRequest {
        http_request: request,
        context,
    };
    let res = client.oneshot(req).await.map_err(|e| {
        match e.downcast::<FetchError>() {
            // Replace the internal subgraph name with the connector label
            Ok(inner) => match *inner {
                FetchError::SubrequestHttpError {
                    status_code,
                    service: _,
                    reason,
                } => Box::new(FetchError::SubrequestHttpError {
                    status_code,
                    service,
                    reason,
                }),
                _ => inner,
            },
            Err(e) => e,
        }
    })?;

    Ok::<_, BoxError>(ConnectorResponse {
        result: ConnectorResult::HttpResponse(res.http_response),
        key,
        debug_request,
    })
}

#[derive(Clone)]
pub(crate) struct ConnectorServiceFactory {
    pub(crate) schema: Arc<Schema>,
    pub(crate) subgraph_schemas: Arc<HashMap<String, Arc<Valid<apollo_compiler::Schema>>>>,
    pub(crate) http_service_factory: Arc<IndexMap<String, HttpClientServiceFactory>>,
    pub(crate) subscription_config: Option<SubscriptionConfig>,
    pub(crate) connectors_by_service_name: Arc<IndexMap<Arc<str>, Connector>>,
}

impl ConnectorServiceFactory {
    pub(crate) fn new(
        schema: Arc<Schema>,
        subgraph_schemas: Arc<HashMap<String, Arc<Valid<apollo_compiler::Schema>>>>,
        http_service_factory: Arc<IndexMap<String, HttpClientServiceFactory>>,
        subscription_config: Option<SubscriptionConfig>,
        connectors_by_service_name: Arc<IndexMap<Arc<str>, Connector>>,
    ) -> Self {
        Self {
            http_service_factory,
            subgraph_schemas,
            schema,
            subscription_config,
            connectors_by_service_name,
        }
    }

    #[cfg(test)]
    pub(crate) fn empty(schema: Arc<Schema>) -> Self {
        Self {
            http_service_factory: Arc::new(Default::default()),
            subgraph_schemas: Default::default(),
            subscription_config: Default::default(),
            connectors_by_service_name: Default::default(),
            schema,
        }
    }
}

impl ServiceFactory<ConnectRequest> for ConnectorServiceFactory {
    type Service = BoxService;

    fn create(&self) -> Self::Service {
        ConnectorService {
            http_service_factory: self.http_service_factory.clone(),
            _schema: self.schema.clone(),
            _subgraph_schemas: self.subgraph_schemas.clone(),
            _subscription_config: self.subscription_config.clone(),
            connectors_by_service_name: self.connectors_by_service_name.clone(),
        }
        .boxed()
    }
}
