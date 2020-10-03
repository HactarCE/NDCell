use crate::gridview::GridView; // required to make `enum_dispatch` happy
use enum_dispatch::enum_dispatch;

/// Methods for managing history entries.
pub trait HistoryManager {
    /// The type used for history entries.
    type HistoryEntry;

    /// Returns a history entry representing the current state.
    fn history_entry(&self) -> Self::HistoryEntry;
    /// Replaces this state with the one recorded in the given history entry and
    /// returns a new history entry representing the state before calling this
    /// method.
    ///
    /// This method is analogous to std::mem::replace().
    fn restore(&mut self, entry: Self::HistoryEntry) -> Self::HistoryEntry;

    /// Returns an immutable reference to the undo stack, with the most recent
    /// entry on top.
    fn undo_stack(&self) -> &Vec<Self::HistoryEntry>;
    /// Returns an immutable reference to the redo stack, with the next entry on
    /// top.
    fn redo_stack(&self) -> &Vec<Self::HistoryEntry>;

    /// Returns a mutable reference to the undo stack, with the most recent
    /// entry on top.
    fn undo_stack_mut(&mut self) -> &mut Vec<Self::HistoryEntry>;
    /// Returns a mutable reference to the redo stack, with the next entry on
    /// top.
    fn redo_stack_mut(&mut self) -> &mut Vec<Self::HistoryEntry>;
}

/// Undo/redo methods -- the "public" interface of HistoryManager. This is
/// automatically implemented for all HistoryManager.
#[enum_dispatch]
pub trait History {
    /// Pushes the current state onto the undo stack and clears the redo stack.
    fn record(&mut self);

    /// Restores the last state from the undo stack, pushing the current state
    /// onto the redo stack.
    ///
    /// Returns true if the undo was successful, or false if there was nothing
    /// to undo.
    fn undo(&mut self) -> bool;
    /// Restores the next state from the redo stack, pushing the current state
    /// onto the undo stack.
    ///
    /// Returns true if the redo was successful, or false if there was nothing
    /// to redo.
    fn redo(&mut self) -> bool;

    /// Returns true if there is something to undo, or false otherwise.
    fn has_undo(&self) -> bool;
    /// Returns true if there is something to redo, or false otherwise.
    fn has_redo(&self) -> bool;
}

impl<T: HistoryManager> History for T {
    fn record(&mut self) {
        let current = self.history_entry();
        // Erase redo history.
        self.redo_stack_mut().clear();
        // Push new entry.
        self.undo_stack_mut().push(current);
    }

    fn undo(&mut self) -> bool {
        if let Some(new_state) = self.undo_stack_mut().pop() {
            let redo_state = self.restore(new_state);
            self.redo_stack_mut().push(redo_state.into());
            true
        } else {
            false
        }
    }
    fn redo(&mut self) -> bool {
        if let Some(new_state) = self.redo_stack_mut().pop() {
            let undo_state = self.restore(new_state);
            self.undo_stack_mut().push(undo_state.into());
            true
        } else {
            false
        }
    }

    fn has_undo(&self) -> bool {
        !self.undo_stack().is_empty()
    }
    fn has_redo(&self) -> bool {
        !self.redo_stack().is_empty()
    }
}
