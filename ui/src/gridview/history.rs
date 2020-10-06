//! Undo/redo history.
//!
//! The simplest model of undo history consists of a single "undo stack." Every
//! operation pushes a history entry for the previous state onto the undo stack.
//! The "undo" action pops a history entry from the top of the undo stack and
//! restores it.
//!
//! To add "redo" functionality, we make an additional "redo stack." To undo an
//! action, we push an entry onto the redo stack and then pop an entry from the
//! undo stack to restore. To redo an action, we push an entry onto the undo
//! stack and then pop an entry from the redo stack to restore. Every operation
//! besides undo/redo clears the redo stack.

use crate::config::Config;
use enum_dispatch::enum_dispatch;

/// Undo/redo stacks to manage undo/redo history.
///
/// `E` is the type used for history entries.
#[derive(Debug)]
pub struct HistoryManager<E> {
    /// Stack of past states, with the most recent on top.
    undo_stack: Vec<E>,
    /// Stack of future states, with the soonest on top.
    redo_stack: Vec<E>,
}
impl<E> Default for HistoryManager<E> {
    fn default() -> Self {
        Self {
            undo_stack: vec![],
            redo_stack: vec![],
        }
    }
}

/// Helper trait that automatically implements History.
pub trait HistoryBase {
    /// The type used for history entries.
    type Entry;

    /// Returns a history entry representing the current state.
    fn history_entry(&self) -> Self::Entry;

    /// Replaces this state with the one recorded in the given history entry and
    /// returns a new history entry representing the state before calling this
    /// method.
    ///
    /// This method is analogous to std::mem::replace().
    fn restore_history_entry(&mut self, config: &Config, entry: Self::Entry) -> Self::Entry;

    /// Returns an immutable reference to the history manager.
    fn as_history(&self) -> &HistoryManager<Self::Entry>;
    /// Returns a mutable reference to the history manager.
    fn as_history_mut(&mut self) -> &mut HistoryManager<Self::Entry>;
}

/// Undo/redo methods.
#[enum_dispatch]
pub trait History {
    /// Pushes the current state onto the undo stack and clears the redo stack.
    fn record(&mut self);

    /// Restores the last state from the undo stack, pushing the current state
    /// onto the redo stack.
    ///
    /// Returns true if the undo was successful, or false if there was nothing
    /// to undo.
    fn undo(&mut self, config: &Config) -> bool;
    /// Restores the next state from the redo stack, pushing the current state
    /// onto the undo stack.
    ///
    /// Returns true if the redo was successful, or false if there was nothing
    /// to redo.
    fn redo(&mut self, config: &Config) -> bool;

    /// Returns true if there is something to undo, or false otherwise.
    fn can_undo(&self) -> bool;
    /// Returns true if there is something to redo, or false otherwise.
    fn can_redo(&self) -> bool;
}

impl<T> History for T
where
    T: HistoryBase,
{
    fn record(&mut self) {
        let new_entry = self.history_entry();
        self.as_history_mut().undo_stack.push(new_entry);
        self.as_history_mut().redo_stack.clear();
    }

    fn undo(&mut self, config: &Config) -> bool {
        if let Some(undone_state) = self.as_history_mut().undo_stack.pop() {
            let original_state = self.restore_history_entry(config, undone_state);
            self.as_history_mut().redo_stack.push(original_state);
            true
        } else {
            false
        }
    }
    fn redo(&mut self, config: &Config) -> bool {
        if let Some(redone_state) = self.as_history_mut().redo_stack.pop() {
            let original_state = self.restore_history_entry(config, redone_state);
            self.as_history_mut().undo_stack.push(original_state);
            true
        } else {
            false
        }
    }

    fn can_undo(&self) -> bool {
        !self.as_history().undo_stack.is_empty()
    }
    fn can_redo(&self) -> bool {
        !self.as_history().redo_stack.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn replace<'a, T>(state: &'a mut T) -> impl 'a + FnMut(T) -> T {
        move |x| std::mem::replace(state, x)
    }

    struct HistoryTester {
        /// Current value.
        curr: i32,
        /// Undo/redo history.
        h: HistoryManager<i32>,
    }
    impl HistoryTester {
        fn new(curr: i32) -> Self {
            Self {
                curr,
                h: HistoryManager::default(),
            }
        }
    }
    impl HistoryBase for HistoryTester {
        type Entry = i32;

        fn history_entry(&self) -> i32 {
            self.curr
        }

        fn restore_history_entry(&mut self, _config: &Config, entry: i32) -> i32 {
            std::mem::replace(&mut self.curr, entry)
        }

        fn as_history(&self) -> &HistoryManager<i32> {
            &self.h
        }

        fn as_history_mut(&mut self) -> &mut HistoryManager<i32> {
            &mut self.h
        }
    }

    #[test]
    fn test_undo_redo() {
        let cfg = Config::default();
        let mut h = HistoryTester::new(10);

        h.record();
        h.curr = 20;

        h.record();
        h.curr = 30;

        /* Try undo() */

        assert!(h.undo(&cfg));
        assert_eq!(20, h.curr);

        assert!(h.undo(&cfg));
        assert_eq!(10, h.curr);

        // no more undo history; should fail
        assert!(!h.undo(&cfg));
        assert_eq!(10, h.curr);

        /* Try redo() */

        assert!(h.redo(&cfg));
        assert_eq!(20, h.curr);

        assert!(h.redo(&cfg));
        assert_eq!(30, h.curr);

        // no more redo history; should fail
        assert!(!h.redo(&cfg));
        assert_eq!(30, h.curr);
    }

    #[test]
    fn test_undo_redo_overwrite() {
        let cfg = Config::default();
        let mut h = HistoryTester::new(10);

        h.record();
        h.curr = 20;

        h.record();
        h.curr = 30;

        h.record();
        h.curr = 40;

        assert!(h.undo(&cfg));
        assert_eq!(30, h.curr);

        assert!(h.undo(&cfg));
        assert_eq!(20, h.curr);

        // should clear redo history but not undo history
        h.record();
        h.curr = 25;

        assert!(h.undo(&cfg));
        assert_eq!(20, h.curr);

        assert!(h.redo(&cfg));
        assert_eq!(25, h.curr);

        // redo history was deleted; should fail
        assert!(!h.redo(&cfg));
        assert_eq!(25, h.curr);
    }
}
