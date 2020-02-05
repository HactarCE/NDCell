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
    /// Returns a mutable reference to the undo stack, with the most recent
    /// entry on top.
    fn undo_stack(&mut self) -> &mut Vec<Self::HistoryEntry>;
    /// Returns a mutable reference to the redo stack, with the next entry on
    /// top.
    fn redo_stack(&mut self) -> &mut Vec<Self::HistoryEntry>;
}

/// Undo/redo methods -- the "public" interface of HistoryManager. This is
/// automatically implemented for all HistoryManager.
pub trait History {
    /// Pushes the current state onto the undo stack and clears the redo stack.
    fn record(&mut self);
    /// Returns true if there is something to undo, or false otherwise.
    fn has_undo(&mut self) -> bool;
    /// Returns true if there is something to redo, or false otherwise.
    fn has_redo(&mut self) -> bool;
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
}

impl<T: HistoryManager> History for T {
    fn record(&mut self) {
        let current = self.history_entry();
        // Erase redo history.
        self.redo_stack().clear();
        // Push new entry.
        self.undo_stack().push(current);
    }
    fn has_undo(&mut self) -> bool {
        // TODO: note that this could take an immut ref, but then there would
        // need to be separate undo_stack() and undo_stack_mut() methods, which
        // just isn't worth the hassle
        !self.undo_stack().is_empty()
    }
    fn has_redo(&mut self) -> bool {
        !self.redo_stack().is_empty()
    }
    fn undo(&mut self) -> bool {
        if let Some(new_state) = self.undo_stack().pop() {
            let redo_state = self.restore(new_state);
            self.redo_stack().push(redo_state.into());
            true
        } else {
            false
        }
    }
    fn redo(&mut self) -> bool {
        if let Some(new_state) = self.redo_stack().pop() {
            let undo_state = self.restore(new_state);
            self.undo_stack().push(undo_state.into());
            true
        } else {
            false
        }
    }
}
