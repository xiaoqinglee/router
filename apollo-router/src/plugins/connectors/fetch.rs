use std::collections::HashMap;
use std::sync::Arc;

use apollo_compiler::name;
use apollo_compiler::NodeStr;
use apollo_federation::schema::ObjectFieldDefinitionPosition;
use apollo_federation::schema::ObjectOrInterfaceFieldDefinitionPosition;
use apollo_federation::schema::ObjectOrInterfaceFieldDirectivePosition;
use apollo_federation::sources::connect;
use apollo_federation::sources::connect::ConnectId;
use apollo_federation::sources::connect::JSONSelection;
use apollo_federation::sources::connect::SubSelection;
use apollo_federation::sources::source;
use apollo_federation::sources::source::SourceId;
use indexmap::IndexMap;

use crate::query_planner::fetch::FetchNode;
use crate::query_planner::fetch::Protocol;
use crate::query_planner::fetch::RestFetchNode;

use super::Connector;

pub(crate) fn convert_connectors(
    connectors: Arc<HashMap<Arc<String>, Connector>>,
) -> IndexMap<SourceId, apollo_federation::sources::connect::Connector> {
    let mut res: IndexMap<SourceId, apollo_federation::sources::connect::Connector> =
        Default::default();

    for (_name, connector) in connectors.iter() {
        res.insert(
            SourceId::Connect(ConnectId {
                label: connector.display_name(),
                subgraph_name: NodeStr::new(connector.origin_subgraph.as_str()),
                directive: fake_directive(),
            }),
            convert_connector(connector),
        );
    }
    res
}

fn fake_directive() -> ObjectOrInterfaceFieldDirectivePosition {
    ObjectOrInterfaceFieldDirectivePosition {
        field: ObjectOrInterfaceFieldDefinitionPosition::Object(ObjectFieldDefinitionPosition {
            type_name: name!("TypeName"),
            field_name: name!("field_name"),
        }),
        directive_name: name!("Directive__name"),
        directive_index: 0,
    }
}

fn convert_connector(connector: &Connector) -> apollo_federation::sources::connect::Connector {
    apollo_federation::sources::connect::Connector {
        id: ConnectId {
            label: connector.api.to_string(),
            subgraph_name: NodeStr::new(connector.origin_subgraph.as_str()),
            directive: fake_directive(),
        },
        selection: connector.transport.selection(),
        transport: connector.transport.clone().into(),
    }
}

impl From<FetchNode> for source::query_plan::FetchNode {
    fn from(value: FetchNode) -> source::query_plan::FetchNode {
        let subgraph_name = match value.protocol.as_ref() {
            Protocol::RestFetch(rf) => rf.parent_service_name.clone().into(),
            _ => value.service_name.clone(),
        };
        source::query_plan::FetchNode::Connect(connect::query_plan::FetchNode {
            source_id: ConnectId {
                label: value.service_name.to_string(),
                subgraph_name,
                directive: fake_directive(),
            },
            field_response_name: name!("data"),
            field_arguments: Default::default(),
            selection: JSONSelection::Named(SubSelection {
                selections: vec![],
                star: None,
            }),
        })
    }
}

impl FetchNode {
    // TODO: let's go all in on nodestr
    pub(crate) fn update_connector_plan(
        &mut self,
        parent_service_name: &String,
        connectors: &Arc<HashMap<Arc<String>, super::Connector>>,
    ) {
        let parent_service_name = parent_service_name.to_string();
        let connector = connectors.get(&self.service_name.to_string()).unwrap();
        let service_name =
            std::mem::replace(&mut self.service_name, connector.display_name().into());
        self.protocol = Arc::new(Protocol::RestFetch(RestFetchNode {
            connector_service_name: service_name.to_string(),
            connector_graph_key: connector._name(),
            parent_service_name,
        }));
        let as_fednext_node: source::query_plan::FetchNode = self.clone().into();
        self.source_node = Some(Arc::new(as_fednext_node));
    }
}
