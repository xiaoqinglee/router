use std::collections::HashSet;
use std::sync::Arc;

use apollo_compiler::validation::Valid;
use apollo_compiler::Schema;
use carryover::carryover_directives;
use indexmap::IndexMap;
use itertools::Itertools;

use crate::error::FederationError;
use crate::link::Link;
use crate::merge::merge_subgraphs;
use crate::schema::FederationSchema;
use crate::sources::connect::ConnectSpec;
use crate::sources::connect::Connector;
use crate::subgraph::Subgraph;
use crate::subgraph::ValidSubgraph;
use crate::ApiSchemaOptions;
use crate::Supergraph;
use crate::ValidFederationSubgraph;

mod carryover;
pub(crate) mod visitors;
use visitors::filter_directives;

pub struct Connectors {
    pub by_service_name: Arc<IndexMap<Arc<str>, Connector>>,
    pub labels_by_service_name: Arc<IndexMap<Arc<str>, String>>,
}

/// The result of a supergraph expansion of connect-aware subgraphs
pub enum ExpansionResult {
    /// The supergraph had some subgraphs that were expanded
    Expanded {
        raw_sdl: String,
        api_schema: Valid<Schema>,
        connectors: Connectors,
    },

    /// The supergraph contained no connect directives and was unchanged.
    Unchanged,
}

/// Expand a schema with connector directives into unique subgraphs per directive
///
/// Until we have a source-aware query planner, work with connectors will need to interface
/// with standard query planning concepts while still enforcing connector-specific rules. To do so,
/// each connector is separated into its own unique subgraph with relevant GraphQL directives to enforce
/// field dependencies and response structures. This allows for satisfiability and validation to piggy-back
/// off of existing functionality in a reproducable way.
pub fn expand_connectors(supergraph_str: &str) -> Result<ExpansionResult, FederationError> {
    // TODO: Don't rely on finding the URL manually to short out
    let connect_url = ConnectSpec::identity();
    let connect_url = format!("{}/{}/v", connect_url.domain, connect_url.name);
    if !supergraph_str.contains(&connect_url) {
        return Ok(ExpansionResult::Unchanged);
    }

    let supergraph = Supergraph::new(supergraph_str)?;
    let api_schema = supergraph.to_api_schema(ApiSchemaOptions {
        // TODO: Get these options from the router?
        include_defer: true,
        include_stream: false,
    })?;

    let (connect_subgraphs, graphql_subgraphs): (Vec<_>, Vec<_>) = supergraph
        .extract_subgraphs()?
        .into_iter()
        .partition_map(
            |(_, sub)| match ConnectSpec::get_from_schema(sub.schema.schema()) {
                Some((spec, link)) if contains_connectors(&link, &sub) => {
                    either::Either::Left((spec, link, sub))
                }
                _ => either::Either::Right(ValidSubgraph::from(sub)),
            },
        );

    // Expand just the connector subgraphs
    let mut expanded_subgraphs = Vec::new();
    let mut spec_versions = HashSet::new();

    for (spec, link, sub) in connect_subgraphs {
        expanded_subgraphs.extend(split_subgraph(&link, sub, spec)?);
        spec_versions.insert(spec);
    }

    // Merge the subgraphs into one supergraph
    let all_subgraphs = graphql_subgraphs
        .iter()
        .chain(expanded_subgraphs.iter().map(|(_, sub)| sub))
        .collect();
    let new_supergraph = merge_subgraphs(all_subgraphs).map_err(|e| {
        FederationError::internal(format!("could not merge expanded subgraphs: {e:?}"))
    })?;

    let mut new_supergraph = FederationSchema::new(new_supergraph.schema.into_inner())?;
    carryover_directives(
        &supergraph.schema,
        &mut new_supergraph,
        spec_versions.into_iter(),
    )
    .map_err(|e| FederationError::internal(format!("could not carry over directives: {e:?}")))?;

    let connectors_by_service_name: IndexMap<Arc<str>, Connector> = expanded_subgraphs
        .into_iter()
        .map(|(connector, sub)| (sub.name.into(), connector))
        .collect();

    let labels_by_service_name = connectors_by_service_name
        .iter()
        .map(|(service_name, connector)| (service_name.clone(), connector.id.label.clone()))
        .collect();

    Ok(ExpansionResult::Expanded {
        raw_sdl: new_supergraph.schema().serialize().to_string(),
        api_schema: api_schema.schema().clone(),
        connectors: Connectors {
            by_service_name: Arc::new(connectors_by_service_name),
            labels_by_service_name: Arc::new(labels_by_service_name),
        },
    })
}

fn contains_connectors(link: &Link, subgraph: &ValidFederationSubgraph) -> bool {
    let connect_name = ConnectSpec::connect_directive_name(link);
    let source_name = ConnectSpec::source_directive_name(link);

    subgraph
        .schema
        .get_directive_definitions()
        .any(|directive| {
            directive.directive_name == connect_name || directive.directive_name == source_name
        })
}

/// Split up a subgraph so that each connector directive becomes its own subgraph.
///
/// Subgraphs passed to this function should contain connector directives.
fn split_subgraph(
    link: &Link,
    subgraph: ValidFederationSubgraph,
    spec: ConnectSpec,
) -> Result<Vec<(Connector, ValidSubgraph)>, FederationError> {
    let connector_map = Connector::from_valid_schema(&subgraph.schema, &subgraph.name, spec)?;

    let expander = helpers::Expander::new(link, &subgraph);
    connector_map
        .into_iter()
        .map(|(id, connector)| {
            // Build a subgraph using only the necessary fields from the directive
            let schema = expander.expand(&connector)?;
            let subgraph = Subgraph::new(
                id.synthetic_name().as_str(),
                &subgraph.url,
                &schema.schema().serialize().to_string(),
            )?;

            // We only validate during debug builds since we should realistically only generate valid schemas
            // for these subgraphs.
            #[cfg(debug_assertions)]
            let schema = subgraph.schema.validate()?;
            #[cfg(not(debug_assertions))]
            let schema = Valid::assume_valid(subgraph.schema);

            Ok((
                connector,
                ValidSubgraph {
                    name: subgraph.name,
                    url: subgraph.url,
                    schema,
                },
            ))
        })
        .try_collect()
}

mod helpers {
    use apollo_compiler::ast;
    use apollo_compiler::ast::Argument;
    use apollo_compiler::ast::Directive;
    use apollo_compiler::ast::FieldDefinition;
    use apollo_compiler::ast::InputValueDefinition;
    use apollo_compiler::ast::Value;
    use apollo_compiler::name;
    use apollo_compiler::schema::Component;
    use apollo_compiler::schema::ComponentName;
    use apollo_compiler::schema::ComponentOrigin;
    use apollo_compiler::schema::DirectiveList;
    use apollo_compiler::schema::EnumType;
    use apollo_compiler::schema::ObjectType;
    use apollo_compiler::schema::ScalarType;
    use apollo_compiler::Name;
    use apollo_compiler::Node;
    use apollo_compiler::Schema;
    use indexmap::IndexMap;
    use indexmap::IndexSet;
    use itertools::Itertools;

    use super::filter_directives;
    use super::visitors::try_insert;
    use super::visitors::try_pre_insert;
    use super::visitors::GroupVisitor;
    use super::visitors::SchemaVisitor;
    use super::FieldAndSelection;
    use crate::error::FederationError;
    use crate::internal_error;
    use crate::link::spec::Identity;
    use crate::link::Link;
    use crate::schema::position::InterfaceFieldDefinitionPosition;
    use crate::schema::position::ObjectFieldDefinitionPosition;
    use crate::schema::position::ObjectOrInterfaceTypeDefinitionPosition;
    use crate::schema::position::ObjectTypeDefinitionPosition;
    use crate::schema::position::PositionLookupError;
    use crate::schema::position::SchemaRootDefinitionKind;
    use crate::schema::position::SchemaRootDefinitionPosition;
    use crate::schema::position::TypeDefinitionPosition;
    use crate::schema::FederationSchema;
    use crate::schema::ValidFederationSchema;
    use crate::sources::connect::json_selection::ExternalVarPaths;
    use crate::sources::connect::variable::Namespace;
    use crate::sources::connect::variable::VariableReference;
    use crate::sources::connect::ConnectSpec;
    use crate::sources::connect::Connector;
    use crate::sources::connect::EntityResolver;
    use crate::sources::connect::JSONSelection;
    use crate::sources::connect::PathSelection;
    use crate::subgraph::spec::EXTERNAL_DIRECTIVE_NAME;
    use crate::subgraph::spec::INTF_OBJECT_DIRECTIVE_NAME;
    use crate::subgraph::spec::KEY_DIRECTIVE_NAME;
    use crate::subgraph::spec::REQUIRES_DIRECTIVE_NAME;
    use crate::supergraph::new_empty_fed_2_subgraph_schema;
    use crate::ValidFederationSubgraph;

    /// A helper struct for expanding a subgraph into one per connect directive.
    pub(super) struct Expander<'a> {
        /// The name of the connect directive, possibly aliased.
        #[allow(unused)]
        connect_name: Name,

        /// The name of the connect directive, possibly aliased.
        #[allow(unused)]
        source_name: Name,

        /// The name of the @key directive, as known in the subgraph
        key_name: Name,

        /// The name of the @interfaceObject directive, as known in the subgraph
        interface_object_name: Name,

        /// The original schema that contains connect directives
        original_schema: &'a ValidFederationSchema,

        /// A list of directives to exclude when copying over types from the
        /// original schema.
        directive_deny_list: IndexSet<Name>,
    }

    impl<'a> Expander<'a> {
        pub(super) fn new(link: &Link, subgraph: &'a ValidFederationSubgraph) -> Expander<'a> {
            let connect_name = ConnectSpec::connect_directive_name(link);
            let source_name = ConnectSpec::source_directive_name(link);

            // When we go to expand all output types, we'll need to make sure that we don't carry over
            // any connect-related directives. The following directives are also special because they
            // influence planning and satisfiability:
            //
            // - @key: derived based on the fields selected
            // - @external: the current approach will only add external fields to the list of keys
            //     if used in the transport. If not used at all, the field marked with this directive
            //     won't even be included in the expanded subgraph, but if it _is_ used then leaving
            //     this directive will result in planning failures.
            // - @requires: the current approach will add required fields to the list of keys for
            //     implicit entities, so it can't stay.
            let key_name = subgraph
                .schema
                .metadata()
                .and_then(|m| m.for_identity(&Identity::federation_identity()))
                .map(|f| f.directive_name_in_schema(&KEY_DIRECTIVE_NAME))
                .unwrap_or(KEY_DIRECTIVE_NAME);
            let interface_object_name = subgraph
                .schema
                .metadata()
                .and_then(|m| m.for_identity(&Identity::federation_identity()))
                .map(|f| f.directive_name_in_schema(&INTF_OBJECT_DIRECTIVE_NAME))
                .unwrap_or(INTF_OBJECT_DIRECTIVE_NAME);
            let extra_excluded = [EXTERNAL_DIRECTIVE_NAME, REQUIRES_DIRECTIVE_NAME]
                .into_iter()
                .map(|d| {
                    subgraph
                        .schema
                        .metadata()
                        .and_then(|m| m.for_identity(&Identity::federation_identity()))
                        .map(|f| f.directive_name_in_schema(&d))
                        .unwrap_or(d)
                });
            let directive_deny_list = IndexSet::from_iter(extra_excluded.chain([
                key_name.clone(),
                connect_name.clone(),
                source_name.clone(),
            ]));

            Self {
                connect_name,
                source_name,
                key_name,
                interface_object_name,
                original_schema: &subgraph.schema,
                directive_deny_list,
            }
        }

        /// Build an expanded subgraph for the supplied connector
        pub(super) fn expand(
            &self,
            connector: &Connector,
        ) -> Result<FederationSchema, FederationError> {
            let mut schema = new_empty_fed_2_subgraph_schema()?;
            let query_alias = self
                .original_schema
                .schema()
                .schema_definition
                .query
                .as_ref()
                .map(|m| m.name.clone())
                .unwrap_or(name!("Query"));

            let field = &connector.id.directive.field;
            let field_def = field.get(self.original_schema.schema())?;
            let field_type = self
                .original_schema
                .get_type(field_def.ty.inner_named_type().clone())?;

            // We'll need to make sure that we always process the inputs first, since they need to be present
            // before any dependent types
            self.process_inputs(&mut schema, &field_def.arguments)?;

            // Actually process the type annotated with the connector, making sure to walk nested types
            match field_type {
                TypeDefinitionPosition::Object(object) => {
                    SchemaVisitor::new(
                        self.original_schema,
                        &mut schema,
                        &self.directive_deny_list,
                    )
                    .walk((
                        object,
                        connector.selection.next_subselection().cloned().ok_or(
                            FederationError::internal("empty selections are not allowed"),
                        )?,
                    ))?;
                }

                TypeDefinitionPosition::Scalar(_) | TypeDefinitionPosition::Enum(_) => {
                    self.insert_custom_leaf(&mut schema, &field_type)?;
                }

                TypeDefinitionPosition::Interface(interface) => {
                    return Err(FederationError::internal(format!(
                        "connect directives not yet supported on interfaces: found on {}",
                        interface.type_name
                    )))
                }
                TypeDefinitionPosition::Union(union) => {
                    return Err(FederationError::internal(format!(
                        "connect directives not yet supported on union: found on {}",
                        union.type_name
                    )))
                }
                TypeDefinitionPosition::InputObject(input) => {
                    return Err(FederationError::internal(format!(
                        "connect directives not yet supported on inputs: found on {}",
                        input.type_name
                    )))
                }
            };

            // Add the root type for this connector, optionally inserting a dummy query root
            // if the connector is not defined within a field on a Query (since a subgraph is invalid
            // without at least a root-level Query)
            let ObjectOrInterfaceTypeDefinitionPosition::Object(parent_object) = field.parent()
            else {
                return Err(FederationError::internal(
                    "connect directives on interfaces is not yet supported",
                ));
            };

            self.insert_query_for_field(&mut schema, &query_alias, &parent_object, field_def)?;

            let root = SchemaRootDefinitionPosition {
                root_kind: SchemaRootDefinitionKind::Query,
            };
            root.insert(
                &mut schema,
                ComponentName {
                    origin: ComponentOrigin::Definition,
                    name: query_alias,
                },
            )?;

            // Process any outputs needed by the connector
            self.process_outputs(
                &mut schema,
                connector,
                parent_object.type_name.clone(),
                field_def.ty.inner_named_type().clone(),
            )?;

            Ok(schema)
        }

        /// Process all input types
        ///
        /// Inputs can include leaf types as well as custom inputs.
        fn process_inputs(
            &self,
            to_schema: &mut FederationSchema,
            arguments: &[Node<InputValueDefinition>],
        ) -> Result<(), FederationError> {
            // All inputs to a connector's field need to be carried over in order to always generate
            // valid subgraphs
            for arg in arguments {
                let arg_type_name = arg.ty.inner_named_type();
                let arg_type = self.original_schema.get_type(arg_type_name.clone())?;
                let arg_extended_type = arg_type.get(self.original_schema.schema())?;

                // If the input type isn't built in, then we need to carry it over, making sure to only walk
                // if we have a complex input since leaf types can just be copied over.
                if !arg_extended_type.is_built_in() {
                    match arg_type {
                        TypeDefinitionPosition::InputObject(input) => SchemaVisitor::new(
                            self.original_schema,
                            to_schema,
                            &self.directive_deny_list,
                        )
                        .walk(input)?,

                        other => self.insert_custom_leaf(to_schema, &other)?,
                    };
                }
            }

            Ok(())
        }

        // Process outputs needed by a connector
        //
        // By the time this method is called, all dependent types should exist for a connector,
        // including its direct inputs. Since each connector could select only a subset of its output
        // type, this method carries over each output type as seen by the selection defined on the connector.
        fn process_outputs(
            &self,
            to_schema: &mut FederationSchema,
            connector: &Connector,
            parent_type_name: Name,
            output_type_name: Name,
        ) -> Result<(), FederationError> {
            let parent_type = self.original_schema.get_type(parent_type_name)?;
            let output_type = to_schema.get_type(output_type_name.clone())?;

            // The body of the request might include references to input arguments / sibling fields
            // that will need to be handled, so we extract any referenced variables now
            let body_parameters = connector
                .transport
                .body
                .as_ref()
                .map(extract_params_from_selection)
                .into_iter()
                .flatten();

            // The URI or headers might reference $this, so we check for that too
            let url_parameters = connector
                .transport
                .connect_template
                .expressions()
                .map(|e| &e.expression)
                .flat_map(extract_params_from_selection);
            // TODO: add a test for header keys
            let header_parameters =
                connector.transport.headers.values().flat_map(|header| {
                    header.expressions().flat_map(extract_params_from_selection)
                });

            // The actual selection might make use of the $this variable, so we grab them too
            let selection_parameters = extract_params_from_selection(&connector.selection);

            let (key_for_type, namespace_filter) =
                if matches!(connector.entity_resolver, Some(EntityResolver::Explicit)) {
                    // `entity: true` connectors only exist on Query.
                    // We don't generate keys for `Query`, these are keys for the output type of the field.
                    // Therefore, we're also only considering the `$args` fields as keys, which should
                    // map 1-1 with output type fields.
                    (output_type, Namespace::Args)
                } else {
                    // We're extending an entity by adding a new field dependent on some other fields
                    // (identified by `$this`).
                    (parent_type, Namespace::This)
                };

            // We'll need to collect all synthesized keys for the output type, adding a federation
            // `@key` directive once completed.
            let mut keys = Vec::new();
            for VariableReference { path, .. } in body_parameters
                .into_iter()
                .chain(url_parameters)
                .chain(header_parameters)
                .chain(selection_parameters)
                .unique()
                .filter(|var| var.namespace.namespace == namespace_filter)
            {
                // Arguments should be added to the synthesized key, since they are mandatory
                // to resolving the output type. The synthesized key should only include the portions
                // of the inputs actually used throughout the selections of the transport.
                //
                // Note that this only applies to connectors marked as an entity resolver, since only
                // those should be allowed to fully resolve a type given the required arguments /
                // synthesized keys.
                //
                // All sibling fields marked by $this in a transport must be carried over to the output type
                // regardless of its use in the output selection.
                let parts = path.iter().map(|part| part.part.as_ref());
                let field_and_selection = FieldAndSelection::from_path(parts);
                let field_name = Name::new(field_and_selection.field_name)?;
                let field: Box<dyn Field> = match &key_for_type {
                    TypeDefinitionPosition::Object(o)  => Box::new(o.field(field_name)),
                    TypeDefinitionPosition::Interface(i) => Box::new(i.field(field_name)),
                    TypeDefinitionPosition::Union(_) | TypeDefinitionPosition::InputObject(_)=> {
                        return Err(FederationError::internal(
                            "siblings of type interface, input object, or union are not yet handled",
                        ))
                    }
                    other => {
                        return Err(FederationError::internal(format!(
                            "cannot select a sibling on a leaf type: {}",
                            other.type_name()
                        )))
                    }
                };

                let field_def = field.get(self.original_schema.schema())?;

                // Mark it as a required key for the output type
                if !field_and_selection.sub_selection.is_empty() {
                    // We'll also need to carry over the output type of this sibling if there is a sub
                    // selection.
                    let field_output = field_def.ty.inner_named_type();
                    if to_schema.try_get_type(field_output.clone()).is_none() {
                        // We use a fake JSONSelection here so that we can reuse the visitor
                        // when generating the output types and their required members.
                        let visitor = SchemaVisitor::new(
                            self.original_schema,
                            to_schema,
                            &self.directive_deny_list,
                        );
                        let parsed = JSONSelection::parse(&field_and_selection.sub_selection)
                            .map_err(|e| {
                                FederationError::internal(format!(
                                    "error creating key from `$this` variable: {e}"
                                ))
                            })?;

                        let output_type = match self
                            .original_schema
                            .get_type(field_output.clone())?
                        {
                            TypeDefinitionPosition::Object(object) => object,

                            other => {
                                return Err(FederationError::internal(format!("connector output types currently only support object types: found {}", other.type_name())))
                            }
                        };

                        visitor.walk((
                            output_type,
                            parsed.next_subselection().cloned().ok_or(
                                FederationError::internal("empty selections are not allowed"),
                            )?,
                        ))?;
                    }
                }

                keys.push(field_and_selection.to_key());

                // Add the field if not already present in the output schema
                if field.get(to_schema.schema()).is_err() {
                    field.insert(
                        to_schema,
                        Component::new(FieldDefinition {
                            description: field_def.description.clone(),
                            name: field_def.name.clone(),
                            arguments: field_def.arguments.clone(),
                            ty: field_def.ty.clone(),
                            directives: filter_directives(
                                &self.directive_deny_list,
                                &field_def.directives,
                            ),
                        }),
                    )?;
                }
            }

            // If we have marked keys as being necessary for this output type, add them as an `@key`
            // directive now.
            if !keys.is_empty() {
                let key_directive = Directive {
                    name: self.key_name.clone(),
                    arguments: vec![Node::new(Argument {
                        name: name!("fields"),
                        value: Node::new(Value::String(keys.join(" "))),
                    })],
                };

                match key_for_type {
                    TypeDefinitionPosition::Object(o) => {
                        o.insert_directive(to_schema, Component::new(key_directive))
                    }
                    TypeDefinitionPosition::Interface(i) => {
                        i.insert_directive(to_schema, Component::new(key_directive))
                    }

                    TypeDefinitionPosition::Scalar(_)
                    | TypeDefinitionPosition::Union(_)
                    | TypeDefinitionPosition::Enum(_)
                    | TypeDefinitionPosition::InputObject(_) => {
                        return Err(FederationError::internal(
                            "keys cannot be added to scalars, unions, enums, or input objects",
                        ))
                    }
                }?;
            } else {
                self.copy_interface_object_keys(output_type_name, to_schema)?;
            }

            Ok(())
        }

        /// If the type has @interfaceObject and it doesn't have a key at this point
        /// we'll need to add a key — this is a requirement for using @interfaceObject.
        /// For now we'll just copy over keys from the original supergraph as resolvable: false
        /// but we need to think through the implications of that.
        fn copy_interface_object_keys(
            &self,
            type_name: Name,
            to_schema: &mut FederationSchema,
        ) -> Result<(), FederationError> {
            let Some(original_output_type) = self.original_schema.schema().get_object(&type_name)
            else {
                return Ok(());
            };

            let is_interface_object = original_output_type
                .directives
                .iter()
                .any(|d| d.name == self.interface_object_name);

            if is_interface_object {
                let pos = ObjectTypeDefinitionPosition {
                    type_name: original_output_type.name.clone(),
                };

                for key in original_output_type
                    .directives
                    .iter()
                    .filter(|d| d.name == self.key_name)
                {
                    let key_fields = key
                        .argument_by_name("fields", self.original_schema.schema())
                        .map_err(|_| internal_error!("@key(fields:) argument missing"))?;
                    let key = Directive {
                        name: key.name.clone(),
                        arguments: vec![
                            Node::new(Argument {
                                name: name!("fields"),
                                value: key_fields.clone(),
                            }),
                            Node::new(Argument {
                                name: name!("resolvable"),
                                value: Node::new(Value::Boolean(false)),
                            }),
                        ],
                    };
                    pos.insert_directive(to_schema, Component::new(key))?;
                }
            }

            Ok(())
        }

        /// Inserts a custom leaf type into the schema
        ///
        /// This errors if called with a non-leaf type.
        fn insert_custom_leaf(
            &self,
            to_schema: &mut FederationSchema,
            r#type: &TypeDefinitionPosition,
        ) -> Result<(), FederationError> {
            match r#type {
                TypeDefinitionPosition::Scalar(scalar) => {
                    let def = scalar.get(self.original_schema.schema())?;
                    let def = ScalarType {
                        description: def.description.clone(),
                        name: def.name.clone(),
                        directives: filter_directives(&self.directive_deny_list, &def.directives),
                    };

                    try_pre_insert!(to_schema, scalar)?;
                    try_insert!(to_schema, scalar, Node::new(def))
                }
                TypeDefinitionPosition::Enum(r#enum) => {
                    let def = r#enum.get(self.original_schema.schema())?;
                    let def = EnumType {
                        description: def.description.clone(),
                        name: def.name.clone(),
                        directives: filter_directives(&self.directive_deny_list, &def.directives),
                        values: def.values.clone(),
                    };

                    try_pre_insert!(to_schema, r#enum)?;
                    try_insert!(to_schema, r#enum, Node::new(def))
                }

                other => Err(FederationError::internal(format!(
                    "expected a leaf, found: {}",
                    other.type_name(),
                ))),
            }
        }

        /// Insert a query root for a connect field
        ///
        /// This method will handle creating a dummy query root as shown below when the
        /// parent type is _not_ a root-level Query to pass schema validation.
        ///
        /// ```graphql
        /// type Query {
        ///   _: ID @shareable @inaccessible
        /// }
        /// ```
        ///
        /// Note: This would probably be better off expanding the query to have
        /// an __entities vs. adding an inaccessible field.
        fn insert_query_for_field(
            &self,
            to_schema: &mut FederationSchema,
            query_alias: &Name,
            field_parent: &ObjectTypeDefinitionPosition,
            field: impl AsRef<FieldDefinition>,
        ) -> Result<(), FederationError> {
            // Prime the query type
            let query = ObjectTypeDefinitionPosition {
                type_name: query_alias.clone(),
            };

            // Now we'll need to know what field to add to the query root. In the case
            // where the parent of the field on the original schema was not the root
            // Query object, the field added is a dummy inaccessible field and the actual
            // parent root is created and upserted. Otherwise, the query will contain the field specified.
            let original = field.as_ref();
            let field = if field_parent.type_name != *query_alias {
                // We'll need to upsert the actual type for the field's parent
                let parent_type = field_parent.get(self.original_schema.schema())?;

                try_pre_insert!(to_schema, field_parent)?;
                let field_def = FieldDefinition {
                    description: original.description.clone(),
                    name: original.name.clone(),
                    arguments: original.arguments.clone(),
                    ty: original.ty.clone(),
                    directives: filter_directives(&self.directive_deny_list, &original.directives),
                };
                try_insert!(
                    to_schema,
                    field_parent,
                    Node::new(ObjectType {
                        description: parent_type.description.clone(),
                        name: parent_type.name.clone(),
                        implements_interfaces: parent_type.implements_interfaces.clone(),
                        directives: filter_directives(
                            &self.directive_deny_list,
                            &parent_type.directives,
                        ),
                        // don't insert field def here. if the type already existed
                        // which happens with circular references, then this defintion
                        // won't be used.
                        fields: Default::default()
                    })
                )?;

                let pos = ObjectFieldDefinitionPosition {
                    type_name: parent_type.name.clone(),
                    field_name: field_def.name.clone(),
                };

                pos.insert(to_schema, field_def.into())?;

                // Return the dummy field to add to the root Query
                FieldDefinition {
                    description: None,
                    name: name!("_"),
                    arguments: Vec::new(),
                    ty: ast::Type::Named(ast::NamedType::new("ID")?),
                    directives: ast::DirectiveList(vec![Node::new(Directive {
                        name: name!("federation__inaccessible"),
                        arguments: Vec::new(),
                    })]),
                }
            } else {
                FieldDefinition {
                    description: original.description.clone(),
                    name: original.name.clone(),
                    arguments: original.arguments.clone(),
                    ty: original.ty.clone(),
                    directives: filter_directives(&self.directive_deny_list, &original.directives),
                }
            };

            // Insert the root Query
            // Note: This should error if Query is already defined, as it shouldn't be
            query.pre_insert(to_schema)?;
            query.insert(
                to_schema,
                Node::new(ObjectType {
                    description: None,
                    name: query_alias.clone(),
                    implements_interfaces: IndexSet::with_hasher(Default::default()),
                    directives: DirectiveList::new(),
                    fields: IndexMap::from_iter([(field.name.clone(), Component::new(field))]),
                }),
            )?;

            Ok(())
        }
    }

    /// Extract all seen parameters from a JSONSelection
    fn extract_params_from_selection(
        selection: &JSONSelection,
    ) -> impl Iterator<Item = VariableReference<Namespace>> + '_ {
        selection
            .external_var_paths()
            .into_iter()
            .flat_map(PathSelection::variable_reference)
    }

    // TODO: contribute some code to `position.rs` to make those types more flexible rather than adding it here
    trait Field {
        fn get<'schema>(
            &self,
            schema: &'schema Schema,
        ) -> Result<&'schema Component<FieldDefinition>, PositionLookupError>;

        fn insert(
            &self,
            schema: &mut FederationSchema,
            field: Component<FieldDefinition>,
        ) -> Result<(), FederationError>;
    }

    impl Field for ObjectFieldDefinitionPosition {
        fn get<'schema>(
            &self,
            schema: &'schema Schema,
        ) -> Result<&'schema Component<FieldDefinition>, PositionLookupError> {
            self.get(schema)
        }

        fn insert(
            &self,
            schema: &mut FederationSchema,
            field: Component<FieldDefinition>,
        ) -> Result<(), FederationError> {
            self.insert(schema, field)
        }
    }

    impl Field for InterfaceFieldDefinitionPosition {
        fn get<'schema>(
            &self,
            schema: &'schema Schema,
        ) -> Result<&'schema Component<FieldDefinition>, PositionLookupError> {
            self.get(schema)
        }

        fn insert(
            &self,
            schema: &mut FederationSchema,
            field: Component<FieldDefinition>,
        ) -> Result<(), FederationError> {
            self.insert(schema, field)
        }
    }
}

/// Represents a field and the subselection of that field, which can then be joined together into
/// a full named selection (which is the same as a key, in simple cases).
struct FieldAndSelection<'a> {
    field_name: &'a str,
    sub_selection: String,
}

impl<'a> FieldAndSelection<'a> {
    /// Extract from a path like `a.b.c` into `a` and `b { c }`
    fn from_path<I: IntoIterator<Item = &'a str>>(parts: I) -> Self {
        let mut parts = parts.into_iter().peekable();
        let field_name = parts.next().unwrap_or_default();
        let mut sub_selection = String::new();
        let mut closing_braces = 0;
        while let Some(sub_path) = parts.next() {
            sub_selection.push_str(sub_path);
            if parts.peek().is_some() {
                sub_selection.push_str(" { ");
                closing_braces += 1;
            }
        }
        for _ in 0..closing_braces {
            sub_selection.push_str(" }");
        }
        FieldAndSelection {
            field_name,
            sub_selection,
        }
    }

    fn to_key(&self) -> String {
        if self.sub_selection.is_empty() {
            self.field_name.to_string()
        } else {
            format!("{} {{ {} }}", self.field_name, self.sub_selection)
        }
    }
}

#[cfg(test)]
mod test_field_and_selection {
    use rstest::rstest;

    #[rstest]
    #[case::simple("a", "a", "")]
    #[case::one("a.b", "a", "b")]
    #[case::two("a.b.c", "a", "b { c }")]
    #[case::three("a.b.c.d", "a", "b { c { d } }")]
    fn from_path(
        #[case] path: &str,
        #[case] expected_field: &str,
        #[case] expected_selection: &str,
    ) {
        let result = super::FieldAndSelection::from_path(path.split('.'));
        assert_eq!(result.field_name, expected_field);
        assert_eq!(result.sub_selection, expected_selection);
    }

    #[rstest]
    #[case::simple("a", "", "a")]
    #[case::one("a", "b", "a { b }")]
    #[case::two("a", "b { c }", "a { b { c } }")]
    fn to_key(#[case] field_name: &str, #[case] sub_selection: &str, #[case] expected_key: &str) {
        let result = super::FieldAndSelection {
            field_name,
            sub_selection: sub_selection.to_string(),
        };
        assert_eq!(result.to_key(), expected_key);
    }
}

#[cfg(test)]
mod tests;
