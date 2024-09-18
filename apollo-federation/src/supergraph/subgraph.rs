use std::fmt;
use std::sync::Arc;

use apollo_compiler::collections::IndexMap;

use crate::error::FederationError;
use crate::error::SingleFederationError;
use crate::schema::FederationSchema;
use crate::schema::ValidFederationSchema;

use super::SubgraphName;

pub(super) struct FederationSubgraph {
    pub(super) name: SubgraphName,
    pub(super) url: String,
    pub(super) schema: FederationSchema,
}

pub(super) struct FederationSubgraphs {
    /// The map is keyed by the subgraph name as a string,
    /// not the SubgraphName type (which would be keyed by pointer identity),
    /// so we can use get(&str) and so you can't add duplicate names with
    /// different pointer identity when building a map manually.
    pub(super) subgraphs: IndexMap<Arc<str>, FederationSubgraph>,
}

impl FederationSubgraphs {
    pub(super) fn new() -> Self {
        FederationSubgraphs {
            subgraphs: IndexMap::default(),
        }
    }

    pub(super) fn add(&mut self, subgraph: FederationSubgraph) -> Result<(), FederationError> {
        let raw_name = subgraph.name.to_cloned_arc();
        if self.subgraphs.contains_key(&raw_name) {
            return Err(SingleFederationError::InvalidFederationSupergraph {
                message: format!("A subgraph named \"{}\" already exists", subgraph.name),
            }
            .into());
        }
        self.subgraphs.insert(raw_name, subgraph);
        Ok(())
    }

    fn get(&self, name: impl AsRef<str>) -> Option<&FederationSubgraph> {
        self.subgraphs.get(name.as_ref())
    }

    pub(super) fn get_mut(&mut self, name: impl AsRef<str>) -> Option<&mut FederationSubgraph> {
        self.subgraphs.get_mut(name.as_ref())
    }

    pub fn iter(&self) -> impl Iterator<Item = &FederationSubgraph> {
        self.subgraphs.values()
    }
}

impl IntoIterator for FederationSubgraphs {
    type Item = FederationSubgraph;
    type IntoIter = indexmap::map::IntoValues<Arc<str>, FederationSubgraph>;

    fn into_iter(self) -> Self::IntoIter {
        self.subgraphs.into_values()
    }
}

// TODO(@goto-bus-stop): consider an appropriate name for this in the public API
// TODO(@goto-bus-stop): should this exist separately from the `crate::subgraph::Subgraph` type?
#[derive(Debug, Clone)]
pub struct ValidFederationSubgraph {
    pub name: SubgraphName,
    pub url: String,
    pub schema: ValidFederationSchema,
}

impl ValidFederationSubgraph {
    /// Internal constructor. In the context of a query planner or composition,
    /// this should be called at most once per subgraph name.
    /// XXX(@goto-bus-stop): maybe this should actually be the API signature for
    /// ValidFederationSubgraphs::add, as we can check the invariant there^
    pub(crate) fn new(
        name: &str,
        url: &str,
        schema: ValidFederationSchema,
    ) -> Self {
        Self {
            name: SubgraphName::new(name),
            url: url.to_string(),
            schema,
        }
    }
}

pub struct ValidFederationSubgraphs {
    /// The map is keyed by the subgraph name as a string,
    /// not the SubgraphName type (which would be keyed by pointer identity),
    /// so we can use get(&str) and so you can't add duplicate names with
    /// different pointer identity when building a map manually.
    pub(super) subgraphs: IndexMap<Arc<str>, ValidFederationSubgraph>,
}

impl fmt::Debug for ValidFederationSubgraphs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ValidFederationSubgraphs ")?;
        f.debug_map().entries(self.subgraphs.iter()).finish()
    }
}

impl ValidFederationSubgraphs {
    pub(crate) fn new() -> Self {
        ValidFederationSubgraphs {
            subgraphs: IndexMap::default(),
        }
    }

    pub(crate) fn add(&mut self, subgraph: ValidFederationSubgraph) -> Result<(), FederationError> {
        let raw_name = subgraph.name.to_cloned_arc();
        if self.subgraphs.contains_key(&raw_name) {
            return Err(SingleFederationError::InvalidFederationSupergraph {
                message: format!("A subgraph named \"{}\" already exists", subgraph.name),
            }
            .into());
        }
        self.subgraphs.insert(raw_name, subgraph);
        Ok(())
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&ValidFederationSubgraph> {
        self.subgraphs.get(name.as_ref())
    }

    pub fn iter(&self) -> impl Iterator<Item = &ValidFederationSubgraph> {
        self.subgraphs.values()
    }
}

impl IntoIterator for ValidFederationSubgraphs {
    type Item = ValidFederationSubgraph;
    type IntoIter = indexmap::map::IntoValues<Arc<str>, ValidFederationSubgraph>;

    fn into_iter(self) -> Self::IntoIter {
        self.subgraphs.into_values()
    }
}
