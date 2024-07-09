use std::fmt::Debug;
use std::sync::Arc;

use apollo_compiler::executable;
use apollo_compiler::Name;
use indexmap::IndexSet;
use itertools::Itertools as _;

use super::Selection;
use super::SelectionId;
use crate::error::FederationError;

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
    CollisionSentinel(SelectionId),
}

impl std::fmt::Display for SelectionKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Field {
                response_name,
                directives,
            } => {
                write!(f, "{response_name}")?;
                if !directives.is_empty() {
                    write!(f, " {directives}")?;
                }
            }
            Self::InlineFragment {
                type_condition,
                directives,
            } => {
                if let Some(type_condition) = type_condition {
                    write!(f, "... on {type_condition}")?;
                } else {
                    write!(f, "...")?;
                }
                if !directives.is_empty() {
                    write!(f, " {directives}")?;
                }
            }
            Self::FragmentSpread {
                fragment_name,
                directives,
            } => {
                write!(f, "...{fragment_name}")?;
                if !directives.is_empty() {
                    write!(f, " {directives}")?;
                }
            }
            Self::Defer { deferred_id } => write!(f, "@defer[{deferred_id:?}]")?,
            Self::CollisionSentinel(id) => write!(f, "COLLISION_SENTINEL[{id:?}]")?,
        }
        Ok(())
    }
}

/// A sorted map of selections.
#[derive(Clone, PartialEq, Eq)]
pub(crate) struct SelectionMap {
    selections: Vec<Selection>,
    keys: IndexSet<SelectionKey>,
}

#[derive(Debug)]
pub(crate) enum ModifySelection {
    /// Keep this selection.
    Keep,
    /// Remove this selection from the map.
    Remove,
    /// Replace this selection by the selections in the provided set.
    Flatten(SelectionSet),
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

    /// Get a reference to a selection that only allows mutations that do not invalidate its key.
    pub(crate) fn get_safe_mut<'map>(
        &'map mut self,
        key: &SelectionKey,
    ) -> Option<SelectionValue<'map>> {
        self.keys
            .get_index_of(key)
            .map(|index| SelectionValue::new(&mut self.selections[index]))
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

    /// Iterate over the selections in the map, allowing mutation only to the parts of the
    /// selection that do not affect the key.
    pub(crate) fn iter_safe_mut(
        &mut self,
    ) -> impl Iterator<Item = (&'_ SelectionKey, SelectionValue<'_>)>
           + ExactSizeIterator
           + DoubleEndedIterator {
        self.keys
            .iter()
            .zip(self.selections.iter_mut().map(SelectionValue::new))
    }

    /// Merge in selections at the given index. New keys are inserted, in order, at `index`.
    /// Selections with keys that are already in the map are merged into those existing selections.
    ///
    /// Returns an adjusted index: 1 past the final selection that this call added.
    fn merge_at(
        &mut self,
        mut index: usize,
        selections: impl Iterator<Item = Selection>,
    ) -> Result<usize, FederationError> {
        for selection in selections {
            let key = selection.key();
            match self.keys.insert_full(key) {
                (new_index, true) => {
                    // Move the key to the right place
                    self.keys.move_index(new_index, index);
                    // Insert the selection in the right place
                    self.selections.insert(index, selection);
                    index += 1;
                }
                (merge_index, false) if merge_index <= index => {
                    match (&mut self.selections[merge_index], selection) {
                        (Selection::Field(old_field), Selection::Field(new_field)) => {
                            FieldSelectionValue::new(old_field)
                                .merge_into(std::iter::once(&*new_field))?;
                        }
                        (
                            Selection::InlineFragment(old_fragment),
                            Selection::InlineFragment(new_fragment),
                        ) => InlineFragmentSelectionValue::new(old_fragment)
                            .merge_into(std::iter::once(&*new_fragment))?,
                        (
                            Selection::FragmentSpread(old_fragment),
                            Selection::FragmentSpread(new_fragment),
                        ) => FragmentSpreadSelectionValue::new(old_fragment)
                            .merge_into(std::iter::once(&*new_fragment))?,
                        // `insert_full` returning `(_, false)` means the selection keys matched,
                        // which also means the kinds of selection must have been the same, so this
                        // branch can never be reached.
                        _ => unreachable!(),
                    }
                }
                (merge_index, false) => {
                    // This is a very special case: we are merging in a selection at a place
                    // *AFTER* the current index. This function is part of the `modify_selections()` API.
                    // That means that we will iterate over the whole selection set and potentially
                    // modify many selections. If we merge an element into a *later* element, that element
                    // has not yet been modified.
                    // This can be a problem. Imagine that the `modify_selections()` call is used
                    // to change the parent type of a whole selection set and also flatten inline fragments.
                    // Now, given `{ a ... { b } b }`:
                    // - `a`'s parent type is changed. This does not change its key. Nothing problematic occurs.
                    // - `... { b }` is flattened, and `b`'s parent type is changed. This changes
                    // the key of the selection. Its new key is `b`. If we tried to merge the new selection
                    // into the existing, final `b`, it would cause an error: the parent types
                    // don't match, because the final `b` hasn't been visited yet.
                    //
                    // I avoid this by giving the final `b` an arbitrary key. Then, once it is
                    // visited, its key will *always* mismatch, and it will be merged back into the
                    // earlier `b` *after* both `b`s have been modified. This seems tricky, but
                    // it's okay:
                    // - During `modify_selections()`, user code cannot access any other fields in
                    // the map, so it can't try to look up the second `b` by key.
                    // - If any *other* selections are also merging into `b`, they will merge into
                    // the first `b`, which has a correct key, not the second `b`, which has an
                    // incorrect key.

                    /*
                                        eprintln!(
                                            "merge_at({index}), {} {}",
                                            self.keys.len(),
                                            self.selections.len()
                                        );
                                        eprintln!(
                                            "  keys: {}",
                                            self.keys.iter().map(|x| format!("{x}")).join(", ")
                                        );
                                        eprintln!(
                                            "  selections: {}",
                                            self.selections.iter().map(|x| format!("{x}")).join(", ")
                                        );
                    */

                    // Move the `merge_index` key to our current index
                    self.keys.move_index(merge_index, index);
                    self.selections.insert(index, selection);

                    // The new insertion necessarily happened *before* `merge_index`: adjust it so
                    // it still points to the same thing.
                    let merge_index = merge_index + 1;

                    // Replace the `merge_index` key by a sentinel value
                    let sentinel_index = self
                        .keys
                        .insert_full(SelectionKey::CollisionSentinel(SelectionId::new()))
                        .0;
                    self.keys.move_index(sentinel_index, merge_index);
                    index += 1;

                    debug_assert_eq!(self.keys.len(), self.selections.len());

                    /*
                                        eprintln!("after: {} {}", self.keys.len(), self.selections.len());
                                        eprintln!(
                                            "  keys: {}",
                                            self.keys.iter().map(|x| format!("{x}")).join(", ")
                                        );
                                        eprintln!(
                                            "  selections: {}",
                                            self.selections.iter().map(|x| format!("{x}")).join(", ")
                                        );
                    */
                }
            }
        }
        Ok(index)
    }

    /// Handle selection replacement or merging after a mutation.
    ///
    /// This function should be called when the selection is in the map, but its key, at the
    /// correct index, is out of date.
    /// If the mutated key is completely new, the old key is replaced by the new key.
    /// If the mutated key collides with another key in the map, the mutated selection is removed
    /// from the map, and merged into the other key's selection.
    ///
    /// `index` is the index where the mutation occurred.
    /// `selection_key` is the new selection key after mutation.
    fn handle_key_mutated(
        &mut self,
        index: usize,
        new_key: SelectionKey,
    ) -> Result<bool, FederationError> {
        match self.keys.insert_full(new_key.clone()) {
            (new_index, true) => {
                // Then move it into place.
                self.keys.move_index(new_index, index);
                // And remove the old key.
                self.keys.pop();
                Ok(true)
            }
            (mut merge_index, false) => {
                assert_ne!(
                    index, merge_index,
                    "handle_key_mutated called but key was the same"
                );

                // Remove the modified selection to merge it with the conflicting selection.
                self.keys.shift_remove_index(index);
                let modified_selection = self.selections.remove(index);

                // We just removed `index`: this may have caused the other selection to
                // shift.
                if index < merge_index {
                    merge_index -= 1;
                }

                let merge_selection = &mut self.selections[merge_index];
                // If this fails there is a bug in the code above.
                debug_assert_eq!(
                    merge_selection.key(),
                    modified_selection.key(),
                    "merge target key must be the same"
                );

                match (merge_selection, modified_selection) {
                    (Selection::Field(old_field), Selection::Field(new_field)) => {
                        FieldSelectionValue::new(old_field)
                            .merge_into(std::iter::once(&*new_field))?;
                    }
                    (
                        Selection::InlineFragment(old_fragment),
                        Selection::InlineFragment(new_fragment),
                    ) => InlineFragmentSelectionValue::new(old_fragment)
                        .merge_into(std::iter::once(&*new_fragment))?,
                    (
                        Selection::FragmentSpread(old_fragment),
                        Selection::FragmentSpread(new_fragment),
                    ) => FragmentSpreadSelectionValue::new(old_fragment)
                        .merge_into(std::iter::once(&*new_fragment))?,
                    // `insert_full` returning `(_, false)` means the selection keys matched,
                    // which also means the kinds of selection must have been the same, so this
                    // branch can never be reached.
                    _ => unreachable!(),
                }

                Ok(false)
            }
        }
    }

    /// Modify selections in the map.
    ///
    /// If an error occurs during modification, this can leave the map in an invalid state. Can we
    /// make this function *consume* self?
    pub(crate) fn modify_selections(
        &mut self,
        mut modify: impl FnMut(&mut Selection) -> Result<ModifySelection, FederationError>,
    ) -> Result<(), FederationError> {
        let mut index = 0;
        while index < self.selections.len() {
            let old_key = self.keys.get_index(index).unwrap().clone();
            let selection = &mut self.selections[index];

            match modify(selection)? {
                ModifySelection::Keep => {
                    let new_key = selection.key();
                    if new_key == old_key {
                        // Simple case: any changes made do not affect the key,
                        // so we do not need to move or merge it
                        index += 1;
                        continue;
                    }

                    if self.handle_key_mutated(index, new_key)? {
                        index += 1;
                    }
                }
                ModifySelection::Flatten(selection_set) => {
                    /*
                                        eprintln!("flatten({selection_set})");
                                        eprintln!(
                                            "  keys: {}",
                                            self.keys.iter().map(|x| format!("{x:?}")).join(", ")
                                        );
                                        eprintln!(
                                            "  selections: {}",
                                            self.selections.iter().map(|x| format!("{x}")).join(", ")
                                        );
                    */

                    // Remove the current selection and replace it by the new selections.
                    self.keys.shift_remove_index(index);
                    self.selections.remove(index);
                    index = self.merge_at(index, selection_set.selections.values().cloned())?;
                }
                ModifySelection::Remove => {
                    self.keys.shift_remove_index(index);
                    self.selections.remove(index);
                }
            }
        }
        Ok(())
    }

    /// Returns the selection set resulting from "recursively" filtering any selection
    /// that does not match the provided predicate.
    /// This method calls `predicate` on every selection of the selection set,
    /// not just top-level ones, and apply a "depth-first" strategy:
    /// when the predicate is called on a given selection it is guaranteed that
    /// filtering has happened on all the selections of its sub-selection.
    pub(super) fn filter_recursive_depth_first(
        &mut self,
        predicate: &mut dyn FnMut(&Selection) -> Result<bool, FederationError>,
    ) -> Result<(), FederationError> {
        fn recur_sub_selections<'sel>(
            selection: &'sel mut Selection,
            predicate: &mut dyn FnMut(&Selection) -> Result<bool, FederationError>,
        ) -> Result<(), FederationError> {
            Ok(match selection {
                Selection::Field(field) => {
                    let field = Arc::make_mut(field);
                    if let Some(sub_selections) = &mut field.selection_set {
                        sub_selections.filter_recursive_depth_first(predicate)?;
                    }
                }
                Selection::InlineFragment(fragment) => {
                    let fragment = Arc::make_mut(fragment);
                    fragment
                        .selection_set
                        .filter_recursive_depth_first(predicate)?;
                }
                Selection::FragmentSpread(_) => {
                    return Err(FederationError::internal("unexpected fragment spread"))
                }
            })
        }

        let mut index = 0;
        while index < self.selections.len() {
            let value = &mut self.selections[index];
            recur_sub_selections(value, predicate)?;
            if predicate(value)? {
                index += 1;
                continue;
            }

            self.keys.shift_remove_index(index);
            self.selections.remove(index);
        }

        Ok(())
    }
}

impl Debug for SelectionMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
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

    pub(crate) fn modify<T>(
        &mut self,
        modify: impl FnOnce(&mut Selection) -> Result<T, FederationError>,
    ) -> Result<T, FederationError> {
        let old_key = self.map.keys.get_index(self.index).unwrap().clone();
        let selection = &mut self.map.selections[self.index];
        let ret = modify(selection)?;
        let modified_key = selection.key();

        // If the key is changed we need to reinsert it.
        if old_key != modified_key {
            self.map.handle_key_mutated(self.index, modified_key)?;
        }

        Ok(ret)
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

use crate::operation::field_selection::FieldSelection;
use crate::operation::fragment_spread_selection::FragmentSpreadSelection;
use crate::operation::inline_fragment_selection::InlineFragmentSelection;
use crate::operation::SelectionSet;
use crate::operation::SiblingTypename;

/// A mutable reference to a `Selection` value in a `SelectionMap`, which
/// also disallows changing key-related data (to maintain the invariant that a value's key is
/// the same as it's map entry's key).
#[derive(Debug)]
pub(crate) enum SelectionValue<'a> {
    Field(FieldSelectionValue<'a>),
    FragmentSpread(FragmentSpreadSelectionValue<'a>),
    InlineFragment(InlineFragmentSelectionValue<'a>),
}

impl<'a> SelectionValue<'a> {
    fn new(selection: &'a mut Selection) -> Self {
        match selection {
            Selection::Field(field_selection) => {
                SelectionValue::Field(FieldSelectionValue::new(field_selection))
            }
            Selection::FragmentSpread(fragment_spread_selection) => SelectionValue::FragmentSpread(
                FragmentSpreadSelectionValue::new(fragment_spread_selection),
            ),
            Selection::InlineFragment(inline_fragment_selection) => SelectionValue::InlineFragment(
                InlineFragmentSelectionValue::new(inline_fragment_selection),
            ),
        }
    }

    pub(super) fn get_directives_mut(&mut self) -> &mut executable::DirectiveList {
        match self {
            Self::Field(field) => field.get_directives_mut(),
            Self::FragmentSpread(spread) => spread.get_directives_mut(),
            Self::InlineFragment(inline) => inline.get_directives_mut(),
        }
    }

    pub(super) fn get_selection_set_mut(&mut self) -> Option<&mut SelectionSet> {
        match self {
            Self::Field(field) => field.get_selection_set_mut().as_mut(),
            Self::FragmentSpread(spread) => Some(spread.get_selection_set_mut()),
            Self::InlineFragment(inline) => Some(inline.get_selection_set_mut()),
        }
    }
}

#[derive(Debug)]
pub(crate) struct FieldSelectionValue<'a>(&'a mut Arc<FieldSelection>);

impl<'a> FieldSelectionValue<'a> {
    pub(crate) fn new(field_selection: &'a mut Arc<FieldSelection>) -> Self {
        Self(field_selection)
    }

    pub(crate) fn get(&self) -> &Arc<FieldSelection> {
        self.0
    }

    pub(crate) fn get_sibling_typename_mut(&mut self) -> &mut Option<SiblingTypename> {
        Arc::make_mut(self.0).field.sibling_typename_mut()
    }

    pub(super) fn get_directives_mut(&mut self) -> &mut executable::DirectiveList {
        Arc::make_mut(self.0).field.directives_mut()
    }

    pub(crate) fn get_selection_set_mut(&mut self) -> &mut Option<SelectionSet> {
        &mut Arc::make_mut(self.0).selection_set
    }
}

#[derive(Debug)]
pub(crate) struct FragmentSpreadSelectionValue<'a>(&'a mut Arc<FragmentSpreadSelection>);

impl<'a> FragmentSpreadSelectionValue<'a> {
    pub(crate) fn new(fragment_spread_selection: &'a mut Arc<FragmentSpreadSelection>) -> Self {
        Self(fragment_spread_selection)
    }

    pub(super) fn get_directives_mut(&mut self) -> &mut executable::DirectiveList {
        &mut Arc::make_mut(self.0).spread.directives
    }

    pub(crate) fn get_selection_set_mut(&mut self) -> &mut SelectionSet {
        &mut Arc::make_mut(self.0).selection_set
    }

    pub(crate) fn get(&self) -> &Arc<FragmentSpreadSelection> {
        self.0
    }
}

#[derive(Debug)]
pub(crate) struct InlineFragmentSelectionValue<'a>(&'a mut Arc<InlineFragmentSelection>);

impl<'a> InlineFragmentSelectionValue<'a> {
    pub(crate) fn new(inline_fragment_selection: &'a mut Arc<InlineFragmentSelection>) -> Self {
        Self(inline_fragment_selection)
    }

    pub(crate) fn get(&self) -> &Arc<InlineFragmentSelection> {
        self.0
    }

    pub(super) fn get_directives_mut(&mut self) -> &mut executable::DirectiveList {
        Arc::make_mut(self.0).inline_fragment.directives_mut()
    }

    pub(crate) fn get_selection_set_mut(&mut self) -> &mut SelectionSet {
        &mut Arc::make_mut(self.0).selection_set
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use apollo_compiler::name;

    use super::*;
    use crate::operation::Field;
    use crate::schema::position::FieldDefinitionPosition;
    use crate::schema::position::ObjectTypeDefinitionPosition;
    use crate::schema::ValidFederationSchema;

    fn field(
        schema: &ValidFederationSchema,
        position: impl Into<FieldDefinitionPosition>,
    ) -> Selection {
        Field::from_position(schema, position.into())
            .with_subselection(None)
            .into()
    }

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
            field(&schema, q.field(name!("a"))),
            field(&schema, q.field(name!("b"))),
            field(&schema, q.field(name!("a"))),
            field(&schema, q.field(name!("c"))),
        ]
        .into_iter()
        .collect();

        let selections = selection_map.values().collect::<Vec<_>>();
        assert_eq!(
            selections,
            [
                &field(&schema, q.field(name!("a"))),
                &field(&schema, q.field(name!("b"))),
                &field(&schema, q.field(name!("c"))),
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
        let a = field(&schema, q.field(name!("a")));
        let b = field(&schema, q.field(name!("b")));
        let c = field(&schema, q.field(name!("c")));
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
        let mut a = Field::from_position(&schema, q.field(name!("a")).into());
        a.alias = Some(name!("alias"));
        assert_eq!(
            selections,
            [
                &a.with_subselection(None).into(),
                &field(&schema, q.field(name!("b"))),
                &field(&schema, q.field(name!("c"))),
            ]
        );
    }

    #[test]
    fn merge_field_selections() {
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
        let a = field(&schema, q.field(name!("a")));
        let b = field(&schema, q.field(name!("b")));
        let c = field(&schema, q.field(name!("c")));
        let mut selection_map: SelectionMap =
            [a.clone(), b.clone(), c.clone()].into_iter().collect();

        selection_map
            .entry(a.key())
            .and_modify(|selection| {
                *selection = Field::from_position(&schema, q.field(name!("b")).into())
                    .with_subselection(None)
                    .into();
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
                &field(&schema, q.field(name!("b"))),
                &field(&schema, q.field(name!("c"))),
            ]
        );
    }

    #[test]
    fn flatten_field_selections() {
        let schema = ValidFederationSchema::parse(
            r#"
            type Query {
                a: Int
                b: Int
                c: Int
                d: Int
                e: Int
                f: Int
            }
        "#,
        )
        .unwrap();

        let q = ObjectTypeDefinitionPosition::new(name!("Query"));
        let a = field(&schema, q.field(name!("a")));
        let b = field(&schema, q.field(name!("b")));
        let c = field(&schema, q.field(name!("c")));
        let mut selection_map: SelectionMap =
            [a.clone(), b.clone(), c.clone()].into_iter().collect();

        selection_map
            .modify_selections(|selection| {
                Ok(ModifySelection::Flatten(SelectionSet::from_raw_selections(
                    selection.schema().clone(),
                    q.clone().into(),
                    [
                        field(&schema, q.field(name!("d"))),
                        field(&schema, q.field(name!("e"))),
                        field(&schema, q.field(name!("f"))),
                    ],
                )))
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
                    response_name: name!("d"),
                    directives: Default::default(),
                },
                &SelectionKey::Field {
                    response_name: name!("e"),
                    directives: Default::default(),
                },
                &SelectionKey::Field {
                    response_name: name!("f"),
                    directives: Default::default(),
                },
            ]
        );

        let selections = selection_map.values().collect::<Vec<_>>();
        assert_eq!(
            selections,
            [
                &field(&schema, q.field(name!("d"))),
                &field(&schema, q.field(name!("e"))),
                &field(&schema, q.field(name!("f"))),
            ]
        );
    }
}
