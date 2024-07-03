use std::sync::Arc;

use apollo_compiler::executable;
use apollo_compiler::Name;
use indexmap::IndexSet;

use crate::error::FederationError;

use super::Selection;
use super::SelectionId;

pub(crate) trait HasSelectionKey {
    fn key(&self) -> SelectionKey;
}

/// A selection "key" (unrelated to the federation `@key` directive) is an identifier of a selection
/// (field, inline fragment, or fragment spread) that is used to determine whether two selections
/// can be merged.
///
/// In order to merge two selections they need to
/// * reference the same field/inline fragment
/// * specify the same directives
/// * directives have to be applied in the same order
/// * directive arguments order does not matter (they get automatically sorted by their names).
/// * selection cannot specify @defer directive
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum SelectionKey {
    Field {
        /// The field alias (if specified) or field name in the resulting selection set.
        response_name: Name,
        /// directives applied on the field
        directives: Arc<executable::DirectiveList>,
    },
    FragmentSpread {
        /// The name of the fragment.
        fragment_name: Name,
        /// Directives applied on the fragment spread (does not contain @defer).
        directives: Arc<executable::DirectiveList>,
    },
    InlineFragment {
        /// The optional type condition of the fragment.
        type_condition: Option<Name>,
        /// Directives applied on the fragment spread (does not contain @defer).
        directives: Arc<executable::DirectiveList>,
    },
    Defer {
        /// Unique selection ID used to distinguish deferred fragment spreads that cannot be merged.
        deferred_id: SelectionId,
    },
}

/// A sorted map of selections.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SelectionMap {
    selections: Vec<Selection>,
    keys: IndexSet<SelectionKey>,
}

impl SelectionMap {
    pub(crate) fn empty() -> Self {
        Self {
            selections: Default::default(),
            keys: Default::default(),
        }
    }

    pub(crate) fn of_single_selection(value: impl Into<Selection>) -> Self {
        let selection = value.into();
        let key = selection.key();
        Self {
            selections: vec![selection],
            keys: std::iter::once(key).collect(),
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.selections.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.selections.is_empty()
    }

    #[deprecated]
    pub(crate) fn clear(&mut self) {
        self.selections.clear();
        self.keys.clear();
    }

    pub(crate) fn insert(&mut self, value: impl Into<Selection>) -> Option<Selection> {
        let selection = value.into();
        let key = selection.key();
        match self.keys.insert_full(key) {
            (index, true) => {
                self.selections.insert(index, selection);
                None
            }
            (index, false) => Some(std::mem::replace(&mut self.selections[index], selection)),
        }
    }

    pub(crate) fn first(&self) -> Option<&Selection> {
        self.selections.first()
    }

    pub(crate) fn get(&self, key: &SelectionKey) -> Option<&Selection> {
        self.keys
            .get_index_of(key)
            .map(|index| &self.selections[index])
    }

    pub(crate) fn contains_key(&self, key: &SelectionKey) -> bool {
        self.keys.contains(key)
    }

    pub(crate) fn entry(&mut self, key: SelectionKey) -> Entry<'_> {
        let (index, is_new) = self.keys.insert_full(key);
        if is_new {
            Entry::Vacant(VacantEntry { map: self, index })
        } else {
            Entry::Occupied(OccupiedEntry { map: self, index })
        }
    }

    pub(crate) fn remove(&mut self, key: &SelectionKey) -> Option<Selection> {
        self.keys
            .shift_remove_full(key)
            .map(|(index, _)| self.selections.remove(index))
    }

    /// Retain only the selections that match the predicate.
    pub(crate) fn retain(&mut self, mut predicate: impl FnMut(&SelectionKey, &Selection) -> bool) {
        let mut index = 0;
        while index < self.selections.len() {
            let key = self.keys.get_index(index).unwrap();
            let value = &self.selections[index];
            if predicate(key, value) {
                index += 1;
                continue;
            }

            self.keys.shift_remove_index(index);
            self.selections.remove(index);
        }
    }

    /// Iterate over the selections in the map.
    pub(crate) fn values(&self) -> std::slice::Iter<'_, Selection> {
        self.selections.iter()
    }

    /// Iterate over the selection keys in the map.
    pub(crate) fn keys(
        &self,
    ) -> impl Iterator<Item = &'_ SelectionKey> + ExactSizeIterator + DoubleEndedIterator {
        self.keys.iter()
    }

    /// Iterate over the selection keys and selections in the map.
    pub(crate) fn iter(
        &self,
    ) -> impl Iterator<Item = (&'_ SelectionKey, &'_ Selection)> + ExactSizeIterator + DoubleEndedIterator
    {
        self.keys().zip(self.values())
    }
}

impl IntoIterator for SelectionMap {
    type Item = (SelectionKey, Selection);
    type IntoIter = std::iter::Zip<
        <IndexSet<SelectionKey> as IntoIterator>::IntoIter,
        <Vec<Selection> as IntoIterator>::IntoIter,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.keys.into_iter().zip(self.selections.into_iter())
    }
}

impl<A> FromIterator<A> for SelectionMap
where
    A: Into<Selection>,
{
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        let mut map = Self::empty();
        for selection in iter {
            map.insert(selection);
        }
        map
    }
}

impl<A> Extend<A> for SelectionMap
where
    A: Into<Selection>,
{
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        for selection in iter {
            self.insert(selection);
        }
    }
}

pub(crate) struct OccupiedEntry<'a> {
    map: &'a mut SelectionMap,
    /// `.map.selections[index]` is guaranteed to exist.
    index: usize,
}

impl<'a> OccupiedEntry<'a> {
    pub(crate) fn get(&self) -> &Selection {
        &self.map.selections[self.index]
    }

    pub(crate) fn modify(
        &mut self,
        modify: impl FnOnce(&mut Selection) -> Result<(), FederationError>,
    ) -> Result<(), FederationError> {
        let old_key = self.map.keys.get_index(self.index).unwrap();
        let selection = &mut self.map.selections[self.index];
        modify(selection)?;
        let new_key = selection.key();

        // If the key is changed we need to reinsert it.
        if *old_key != new_key {
            // First insert the new key.
            let (new_index, true) = self.map.keys.insert_full(new_key) else {
                panic!("new key is not new. the selection map is now in an invalid state.");
            };
            // Then move it into place.
            self.map.keys.swap_indices(self.index, new_index);
            // And remove the old key.
            self.map.keys.pop();
        }

        Ok(())
    }
}

pub(crate) struct VacantEntry<'a> {
    map: &'a mut SelectionMap,
    index: usize,
}

impl<'a> VacantEntry<'a> {
    pub(crate) fn insert(self, selection: Selection) {
        self.map.selections.insert(self.index, selection);
        // Prevent the key from being removed again.
        std::mem::forget(self);
    }

    fn remove_vacant_element(&mut self) {
        // XXX(@goto-bus-stop): maybe more appropriately an assert
        if self.map.keys.len() == self.map.selections.len() + 1 {
            self.map.keys.shift_remove_index(self.index);
        }
    }
}

impl<'a> Drop for VacantEntry<'a> {
    fn drop(&mut self) {
        // If we did not end up using the `VacantEntry` instance,
        // we must remove the element that was added to the SelectionMap's
        // keys so the keys and selections stay in sync.
        self.remove_vacant_element();
    }
}

pub(crate) enum Entry<'a> {
    Occupied(OccupiedEntry<'a>),
    Vacant(VacantEntry<'a>),
}

impl<'a> Entry<'a> {
    pub(crate) fn is_occupied(&self) -> bool {
        matches!(self, Self::Occupied(_))
    }
    pub(crate) fn is_vacant(&self) -> bool {
        matches!(self, Self::Vacant(_))
    }

    pub(crate) fn and_modify(
        &mut self,
        modify: impl FnOnce(&mut Selection) -> Result<(), FederationError>,
    ) -> Result<(), FederationError> {
        if let Entry::Occupied(occupied) = self {
            occupied.modify(modify)
        } else {
            Ok(())
        }
    }
    pub(crate) fn or_insert(
        self,
        produce: impl FnOnce() -> Result<Selection, FederationError>,
    ) -> Result<(), FederationError> {
        if let Entry::Vacant(vacant) = self {
            vacant.insert(produce()?);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use apollo_compiler::name;

    use crate::operation::Field;
    use crate::schema::position::ObjectTypeDefinitionPosition;
    use crate::schema::ValidFederationSchema;

    use super::*;

    #[test]
    fn some_duplicate_field_selections() {
        let schema = ValidFederationSchema::parse(
            r#"
            type Query {
                a: Int
                b: Int
                c: Int
            }
        "#,
        )
        .unwrap();

        let q = ObjectTypeDefinitionPosition::new(name!("Query"));
        let selection_map: SelectionMap = [
            Field::from_position(&schema, q.field(name!("a")).into()).with_subselection(None),
            Field::from_position(&schema, q.field(name!("b")).into()).with_subselection(None),
            Field::from_position(&schema, q.field(name!("a")).into()).with_subselection(None),
            Field::from_position(&schema, q.field(name!("c")).into()).with_subselection(None),
        ]
        .into_iter()
        .collect();

        let selections = selection_map.values().collect::<Vec<_>>();
        assert_eq!(
            selections,
            [
                &Selection::from_field(
                    Field::from_position(&schema, q.field(name!("a")).into()),
                    None
                ),
                &Selection::from_field(
                    Field::from_position(&schema, q.field(name!("b")).into()),
                    None
                ),
                &Selection::from_field(
                    Field::from_position(&schema, q.field(name!("c")).into()),
                    None
                ),
            ]
        );
    }

    #[test]
    fn mutate_field_selections() {
        let schema = ValidFederationSchema::parse(
            r#"
            type Query {
                a: Int
                b: Int
                c: Int
            }
        "#,
        )
        .unwrap();

        let q = ObjectTypeDefinitionPosition::new(name!("Query"));
        let a = Field::from_position(&schema, q.field(name!("a")).into()).with_subselection(None);
        let b = Field::from_position(&schema, q.field(name!("b")).into()).with_subselection(None);
        let c = Field::from_position(&schema, q.field(name!("c")).into()).with_subselection(None);
        let mut selection_map: SelectionMap =
            [a.clone(), b.clone(), c.clone()].into_iter().collect();

        selection_map
            .entry(a.key())
            .and_modify(|selection| {
                match selection {
                    Selection::Field(field) => {
                        let field = Arc::make_mut(field);
                        *field = field
                            .with_updated_alias(name!("alias"))
                            .with_subselection(None);
                    }
                    _ => (),
                }
                Ok(())
            })
            .unwrap();

        assert!(
            selection_map.entry(a.key()).is_vacant(),
            "The key was changed and should have been updated"
        );

        let keys = selection_map.keys().collect::<Vec<_>>();
        assert_eq!(
            keys,
            [
                &SelectionKey::Field {
                    response_name: name!("alias"),
                    directives: Default::default(),
                },
                &SelectionKey::Field {
                    response_name: name!("b"),
                    directives: Default::default(),
                },
                &SelectionKey::Field {
                    response_name: name!("c"),
                    directives: Default::default(),
                },
            ]
        );

        let selections = selection_map.values().collect::<Vec<_>>();
        assert_eq!(
            selections,
            [
                &Selection::from_field(
                    Field::from_position(&schema, q.field(name!("a")).into()),
                    None
                ),
                &Selection::from_field(
                    Field::from_position(&schema, q.field(name!("b")).into()),
                    None
                ),
                &Selection::from_field(
                    Field::from_position(&schema, q.field(name!("c")).into()),
                    None
                ),
            ]
        );
    }
}
