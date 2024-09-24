#[cfg(test)]
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::sync::Arc;

use router_bridge::introspect::IntrospectionError;
use router_bridge::planner::Planner;
use tower::BoxError;

use crate::cache::storage::CacheStorage;
use crate::graphql::Response;
use crate::query_planner::QueryPlanResult;

const DEFAULT_INTROSPECTION_CACHE_CAPACITY: NonZeroUsize =
    unsafe { NonZeroUsize::new_unchecked(5) };

pub(crate) async fn default_cache_storage() -> CacheStorage<String, Response> {
    // This cannot fail as redis is not used.
    CacheStorage::new(DEFAULT_INTROSPECTION_CACHE_CAPACITY, None, "introspection")
        .await
        .expect("failed to create cache storage")
}

/// A cache containing our well known introspection queries.
pub(crate) struct Introspection {
    cache: CacheStorage<String, Response>,
    pub(crate) planner: Arc<Planner<QueryPlanResult>>,
}

impl Introspection {
    pub(crate) async fn with_cache(
        planner: Arc<Planner<QueryPlanResult>>,
        cache: CacheStorage<String, Response>,
    ) -> Result<Self, BoxError> {
        Ok(Self { cache, planner })
    }

    #[cfg(test)]
    pub(crate) async fn from_cache(
        planner: Arc<Planner<QueryPlanResult>>,
        cache: HashMap<String, Response>,
    ) -> Result<Self, BoxError> {
        let this = Self::with_cache(
            planner,
            CacheStorage::new(cache.len().try_into().unwrap(), None, "introspection").await?,
        )
        .await?;

        for (query, response) in cache.into_iter() {
            this.cache.insert(query, response).await;
        }
        Ok(this)
    }

    /// Execute an introspection and cache the response.
    pub(crate) async fn execute(&self, query: String) -> Result<Response, IntrospectionError> {
        if let Some(response) = self.cache.get(&query, |_| Ok(())).await {
            return Ok(response);
        }

        // Do the introspection query and cache it
        let response =
            self.planner
                .introspect(query.clone())
                .await
                .map_err(|_e| IntrospectionError {
                    message: String::from("cannot find the introspection response").into(),
                })?;

        let introspection_result = response.into_result().map_err(|err| IntrospectionError {
            message: format!(
                "introspection error : {}",
                err.into_iter()
                    .map(|err| err.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
            )
            .into(),
        })?;

        let response = Response::builder().data(introspection_result).build();

        self.cache.insert(query, response.clone()).await;

        Ok(response)
    }
}

#[cfg(test)]
mod introspection_tests {
    use std::sync::Arc;

    use router_bridge::planner::IncrementalDeliverySupport;
    use router_bridge::planner::QueryPlannerConfig;

    use super::*;

    #[tokio::test]
    async fn test_plan_cache() {
        let query_to_test = r#"{
            __schema {
              types {
                name
              }
            }
          }"#;
        let schema = include_str!("../tests/fixtures/supergraph.graphql");
        let expected_data = Response::builder().data(42).build();

        let planner = Arc::new(
            Planner::new(
                schema.to_string(),
                QueryPlannerConfig {
                    incremental_delivery: Some(IncrementalDeliverySupport {
                        enable_defer: Some(true),
                    }),
                    graphql_validation: true,
                    reuse_query_fragments: Some(false),
                    generate_query_fragments: None,
                    debug: None,
                    type_conditioned_fetching: false,
                },
            )
            .await
            .unwrap(),
        );

        let cache = [(query_to_test.to_string(), expected_data.clone())]
            .iter()
            .cloned()
            .collect();
        let introspection = Introspection::from_cache(planner, cache).await.unwrap();

        assert_eq!(
            expected_data,
            introspection
                .execute(query_to_test.to_string())
                .await
                .unwrap()
        );
    }
}
