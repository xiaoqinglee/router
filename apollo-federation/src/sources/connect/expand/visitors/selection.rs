use std::ops::Deref;

use apollo_compiler::ast::FieldDefinition;
use apollo_compiler::schema::Component;
use apollo_compiler::schema::EnumType;
use apollo_compiler::schema::ObjectType;
use apollo_compiler::schema::ScalarType;
use apollo_compiler::Name;
use apollo_compiler::Node;
use indexmap::IndexMap;
use itertools::Itertools;

use super::filter_directives;
use super::try_insert;
use super::try_pre_insert;
use super::FieldVisitor;
use super::GroupVisitor;
use super::SchemaVisitor;
use crate::error::FederationError;
use crate::schema::position::ObjectTypeDefinitionPosition;
use crate::schema::position::TypeDefinitionPosition;
use crate::sources::connect::selection::Group;
use crate::sources::connect::selection::SelectionField;

/// Type alias for JSONSelection group info
///
/// A JSONSelection has subselections which do not have a way to lookup the parent subselection
/// nor the field name corresponding to that selection, so we need to keep the matching schema object
/// type when validating selections against concrete types.
pub(crate) type GroupWithPosition<'a> = (ObjectTypeDefinitionPosition, Group<'a>);

impl<'a> FieldVisitor<SelectionField<'a>>
    for SchemaVisitor<'_, ObjectTypeDefinitionPosition, ObjectType>
{
    type Error = FederationError;

    fn visit(&mut self, field: SelectionField<'a>) -> Result<(), Self::Error> {
        let (definition, r#type) = self.type_stack.last_mut().ok_or(FederationError::internal(
            "tried to visit a field in a group not yet entered",
        ))?;

        // Get the type of the field so we know how to visit it
        let field_name = Name::new(field.name)?;
        let field = definition
            .field(field_name.clone())
            .get(self.original_schema.schema())?;
        let field_type = self
            .original_schema
            .get_type(field.ty.inner_named_type().clone())?;
        let extended_field_type = field_type.get(self.original_schema.schema())?;

        // We only need to care about the type of the field if it isn't built-in
        if !extended_field_type.is_built_in() {
            match field_type {
                TypeDefinitionPosition::Scalar(scalar) => {
                    let def = scalar.get(self.original_schema.schema())?;
                    let def = ScalarType {
                        description: def.description.clone(),
                        name: def.name.clone(),
                        directives: filter_directives(self.directive_deny_list, &def.directives),
                    };

                    try_pre_insert!(self.to_schema, scalar)?;
                    try_insert!(self.to_schema, scalar, Node::new(def))?;
                }
                TypeDefinitionPosition::Enum(r#enum) => {
                    let def = r#enum.get(self.original_schema.schema())?;
                    let def = EnumType {
                        description: def.description.clone(),
                        name: def.name.clone(),
                        directives: filter_directives(self.directive_deny_list, &def.directives),
                        values: def.values.clone(),
                    };

                    try_pre_insert!(self.to_schema, r#enum)?;
                    try_insert!(self.to_schema, r#enum, Node::new(def))?;
                }

                // This will be handled by the rest of the visitor
                TypeDefinitionPosition::Object(_) => {}

                // These will be handled later
                TypeDefinitionPosition::Union(_) => {
                    return Err(FederationError::internal(
                        "unions are not yet handled for expansion",
                    ))
                }

                // Anything else is not supported
                TypeDefinitionPosition::InputObject(input) => {
                    return Err(FederationError::internal(format!(
                        "expected field to be a leaf or object type, found: input {}",
                        input.type_name,
                    )))
                }
                TypeDefinitionPosition::Interface(interface) => {
                    return Err(FederationError::internal(format!(
                        "expected field to be a leaf or object type, found: interface {}",
                        interface.type_name,
                    )))
                }
            };
        }

        // Add the field to the currently processing object, making sure to not overwrite if it already
        // exists (and verify that we didn't change the type)
        let new_field = FieldDefinition {
            description: field.description.clone(),
            name: field.name.clone(),
            arguments: field.arguments.clone(),
            ty: field.ty.clone(),
            directives: filter_directives(self.directive_deny_list, &field.directives),
        };
        if let Some(old_field) = r#type.fields.get(&field_name) {
            if *old_field.deref().deref() != new_field {
                return Err(FederationError::internal(
                   format!( "tried to write field to existing type, but field type was different. expected {new_field:?} found {old_field:?}"),
                ));
            }
        } else {
            r#type.fields.insert(field_name, Component::new(new_field));
        }

        Ok(())
    }
}

impl<'a> GroupVisitor<GroupWithPosition<'a>, SelectionField<'a>>
    for SchemaVisitor<'_, ObjectTypeDefinitionPosition, ObjectType>
{
    fn get_group_fields(
        &self,
        (_, group): GroupWithPosition<'a>,
    ) -> Result<Vec<SelectionField<'a>>, <Self as FieldVisitor<SelectionField>>::Error> {
        Ok(group
            .fields()
            .map_err(|err| FederationError::internal(err.to_string()))?
            .iter()
            .sorted_by_key(|field| field.name)
            .cloned()
            .collect())
    }

    fn try_get_group_for_field(
        &self,
        field: &SelectionField<'a>,
    ) -> Result<Option<GroupWithPosition<'a>>, FederationError> {
        let (definition, _) = self.type_stack.last().ok_or(FederationError::internal(
            "tried to get fields on a group not yet visited",
        ))?;
        let field_name = Name::new(field.name)?;

        let field_type_name = definition
            .field(field_name)
            .get(self.original_schema.schema())?
            .ty
            .inner_named_type();

        let TypeDefinitionPosition::Object(field_type) =
            self.original_schema.get_type(field_type_name.clone())?
        else {
            return Ok(None);
        };

        Ok(field.group.as_ref().map(|group| (field_type, *group)))
    }

    fn enter_group(
        &mut self,
        (group_type, group): GroupWithPosition<'a>,
    ) -> Result<Vec<SelectionField<'a>>, FederationError> {
        try_pre_insert!(self.to_schema, group_type)?;
        let def = group_type.get(self.original_schema.schema())?;

        let sub_type = ObjectType {
            description: def.description.clone(),
            name: def.name.clone(),
            implements_interfaces: def.implements_interfaces.clone(),
            directives: filter_directives(self.directive_deny_list, &def.directives),
            fields: IndexMap::with_hasher(Default::default()), // Will be filled in by the `visit` method for each field
        };

        self.type_stack.push((group_type.clone(), sub_type));
        self.get_group_fields((group_type, group))
    }

    fn exit_group(&mut self) -> Result<(), FederationError> {
        let (definition, r#type) = self.type_stack.pop().ok_or(FederationError::internal(
            "tried to exit a group not yet entered",
        ))?;

        try_insert!(self.to_schema, definition, Node::new(r#type))
    }
}

#[cfg(test)]
mod tests {
    // TODO: Write these tests
}
