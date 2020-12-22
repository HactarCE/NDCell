use anyhow::Result;
use log::{error, trace};
use parking_lot::{Condvar, MappedMutexGuard, Mutex, MutexGuard};
use std::error::Error;
use std::fmt;
use std::sync::Arc;
use std::time::Duration;

use ndcell_core::prelude::*;

use super::selection::{Selection2D, Selection3D};

/// Request to compute something in the background.
pub type WorkFn = Box<dyn Send + FnOnce(WorkerHook<'_>) -> WorkerResult>;

/// Progress report on a background operation.
pub enum WorkerProgressReport {
    /// New simulation state.
    NewValues(NewGridViewValues),
}

/// Result of a fallible background operation.
pub type WorkerResult = Result<NewGridViewValues>;
/// Modifications to the gridview resulting from a background operation.
///
/// Each field is `Some` if the operation modifies that attribute of the
/// gridview, or `None` if it does not.
#[derive(Debug, Default)]
pub struct NewGridViewValues {
    pub elapsed: Duration,

    pub clipboard: Option<String>,
    pub automaton: Option<Automaton>,
    pub selection_2d: Option<Option<Selection2D>>,
    pub selection_3d: Option<Option<Selection3D>>,
}

/// Data from the worker thread, which either communicates progress or the final
/// result of an operation.
pub enum WorkerData {
    None,
    Progress(WorkerProgressReport),
    Result(WorkerResult),
}

/// Worker thread state machine.
///
/// FSM diagram: https://i.imgur.com/T5A7hpK.png (not including `Dropped` state)
enum State {
    /// There is no running task.
    ///
    /// - Work requests lead to the `WorkRequested` state and wake the worker
    ///   thread.
    /// - `reset()` does nothing.
    Idle,

    /// The main thread is requesting the worker thread to do some work.
    ///
    /// - Work requests are rejected.
    /// - `reset()` switches to the `Idle` state.
    /// - The worker thread may take the work, leading to the `Working` state.
    WorkRequested(WorkFn),

    /// The worker thread is working. This variant contains an enum describing
    /// the type of work, along with an optional progress report.
    ///
    /// - Work requests are rejected.
    /// - `reset()` drops the worker and creates a new one in the `Idle` state.
    /// - `take()` returns the progress report, replacing it with
    ///   `WorkerProgress::None`.
    /// - The worker thread may finish the work, leading to the `Done` state.
    Working(Option<WorkerProgressReport>),

    /// The worker thread is done with its work. This variant contains the
    /// result of the work.
    ///
    /// - Work requests are rejected.
    /// - `reset()` switches to the `Idle` state, discarding the work result.
    /// - `take()` returns the work result, leading to the `Idle` state.
    Done(WorkerResult),

    /// The `WorkerThread` object has been dropped; the thread itself should
    /// free all resources and exit.
    ///
    /// If the main thread drops the `WorkerThread` at any time, it enters this
    /// state immediately. The worker thread may finish or cancel the work, and
    /// then exit.
    Dropped,
}
impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            State::Idle => write!(f, "State::Idle"),
            State::WorkRequested(_) => write!(f, "State::WorkRequested"),
            State::Working(_) => write!(f, "State::Working"),
            State::Done(_) => write!(f, "State::Done"),
            State::Dropped => write!(f, "State::Dropped"),
        }
    }
}
impl State {
    fn is_idle(&self) -> bool {
        matches!(self, Self::Idle)
    }
}

/// Error returned when an operation fails because the worker is busy.
#[derive(Debug)]
pub struct WorkerBusy;
impl fmt::Display for WorkerBusy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Worker thread is busy")
    }
}
impl Error for WorkerBusy {}
/// Error returned when an operation fails because the worker is idle.
#[derive(Debug)]
pub struct WorkerIdle;
impl fmt::Display for WorkerIdle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Worker thread is idle")
    }
}
impl Error for WorkerIdle {}

/// Interface to another thread that can do work in the background, one task at
/// a time.
#[derive(Debug)]
pub struct WorkerThread(Arc<(Mutex<State>, Condvar)>);
impl Drop for WorkerThread {
    fn drop(&mut self) {
        trace!("Dropping worker thread");
        let (lock, condvar) = &*self.0;
        let mut state = lock.lock();
        *state = State::Dropped;
        condvar.notify_one();
        drop(state);
    }
}
impl Default for WorkerThread {
    fn default() -> Self {
        Self::new()
    }
}
impl WorkerThread {
    pub fn new() -> Self {
        let lock_and_condvar = Arc::new((Mutex::new(State::Idle), Condvar::new()));

        let lock_and_condvar_2 = Arc::clone(&lock_and_condvar);
        std::thread::spawn(move || {
            let (lock, condvar) = &*lock_and_condvar_2;
            let mut state = lock.lock();
            loop {
                match std::mem::replace(&mut *state, State::Idle) {
                    s @ State::Idle | s @ State::Done { .. } => {
                        *state = s;
                        // Wait for the main thread to send more work.
                        condvar.wait(&mut state);
                    }
                    State::WorkRequested(work_fn) => {
                        *state = State::Working(None);
                        drop(state); // unlock mutex while working
                        let result = work_fn(WorkerHook { lock, condvar });
                        state = lock.lock(); // lock mutex again
                        if matches!(*state, State::Working(_)) {
                            *state = State::Done(result);
                        }
                    }
                    State::Working { .. } => {
                        // This should not happen, because the `Working` state
                        // should only exist while this thread is doing work.
                        error!("Worker thread woken in state {:?}", state);
                        break;
                    }
                    State::Dropped => break, // Break out of the loop and exit the thread
                }
            }
            trace!("Worker thread exited");
        });

        Self(lock_and_condvar)
    }

    pub fn is_busy(&self) -> bool {
        !self.is_idle()
    }
    pub fn is_idle(&self) -> bool {
        let (lock, _condvar) = &*self.0;
        lock.lock().is_idle()
    }

    pub fn request(&mut self, work_fn: WorkFn) -> Result<(), WorkerBusy> {
        let (lock, condvar) = &*self.0;
        let mut state = lock.lock();
        if state.is_idle() {
            *state = State::WorkRequested(work_fn);
            condvar.notify_one();
            drop(state);
            Ok(())
        } else {
            Err(WorkerBusy)
        }
    }
    pub fn take_data(&mut self) -> Result<WorkerData, WorkerIdle> {
        let (lock, condvar) = &*self.0;
        let mut state = lock.lock();
        match std::mem::replace(&mut *state, State::Idle) {
            State::Working(Some(progress)) => {
                *state = State::Working(None);
                condvar.notify_one(); // Wake the worker thread, in case it was
                                      // waiting for the progress report to be
                                      // taken.
                Ok(WorkerData::Progress(progress))
            }
            State::Working(None) => {
                *state = State::Working(None);
                Ok(WorkerData::None)
            }
            State::Done(result) => Ok(WorkerData::Result(result)),
            State::Dropped => {
                *state = State::Dropped;
                unreachable!("try_get_result() called on worker thread in dropped state")
            }
            s => {
                *state = s; // unchanged
                Err(WorkerIdle)
            }
        }
    }
    pub fn peek_progress<'a>(
        &'a mut self,
    ) -> Result<MappedMutexGuard<'a, Option<WorkerProgressReport>>, WorkerIdle> {
        let (lock, _condvar) = &*self.0;
        MutexGuard::try_map(lock.lock(), |state| match state {
            State::Working(progress) => Some(progress),
            _ => None,
        })
        .map_err(|_| WorkerIdle)
    }

    /// Cancels any running task and spawns a new worker thread if necessary,
    /// discarding any results that haven't been taken. Returns `true` if the
    /// worker thread was busy or `false` if the worker thread was idle (in
    /// which case the call had no effect).
    pub fn reset(&mut self) -> bool {
        let (lock, _condvar) = &*self.0;
        let mut state = lock.lock();
        match *state {
            State::Idle => return false,
            State::WorkRequested(_) | State::Done(_) => *state = State::Idle,
            State::Working(_) => *state = State::Dropped,
            State::Dropped => unreachable!("reset() called on worker thread in dropped state"),
        }
        drop(state);

        // Reset the worker thread, so that a new task can be requested
        // immediately.
        *self = Self::new();
        true
    }
}

#[derive(Debug)]
pub struct WorkerHook<'a> {
    lock: &'a Mutex<State>,
    condvar: &'a Condvar,
}
impl WorkerHook<'_> {
    pub fn wants_cancel(&self) -> bool {
        let state = self.lock.lock();
        match &*state {
            State::Working(_) => false,
            State::Dropped => true,

            _ => {
                error!("Worker thread hook called in state {:?}", state);
                true
            }
        }
    }
    pub fn progress_report_blocking(&self, progress: WorkerProgressReport) {
        let mut state = self.lock.lock();
        loop {
            match &*state {
                State::Working(Some(_)) => self.condvar.wait(&mut state),
                State::Working(None) => {
                    *state = State::Working(Some(progress));
                    break;
                }
                State::Dropped => break,

                _ => {
                    error!("Worker thread hook called in state {:?}", state);
                    break;
                }
            }
        }
    }
    pub fn progress_report_overwrite(&self, progress: WorkerProgressReport) {
        let mut state = self.lock.lock();
        match &*state {
            State::Working(_) => *state = State::Working(Some(progress)),
            State::Dropped => (),

            _ => error!("Worker thread hook called in state {:?}", state),
        }
    }
}
