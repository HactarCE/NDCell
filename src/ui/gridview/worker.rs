use log::trace;
use num::BigInt;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

use crate::automaton::NdSimulate;

pub enum WorkerRequest {
    Step(BigInt),
    SimContinuous(BigInt),
}

pub struct WorkerResult<T> {
    pub result: T,
    pub record: bool,
    pub time: Duration,
}

/// A manager for worker threads that compute simulation results concurrently.
pub struct Worker<T: NdSimulate + Clone + Send> {
    requests_tx: mpsc::Sender<WorkerRequest>,
    results_rx: mpsc::Receiver<WorkerResult<T>>,
    request_count: usize,
}

impl<T: 'static + NdSimulate + Clone + Send> Worker<T> {
    pub fn new(mut simulation: T) -> Self {
        let (requests_tx, requests_rx) = mpsc::channel();
        let (results_tx, results_rx) = mpsc::sync_channel(1);
        thread::spawn(move || loop {
            loop {
                let continuous: bool;
                let step_size: BigInt;
                match requests_rx.recv() {
                    Ok(WorkerRequest::Step(requested_step_size)) => {
                        continuous = false;
                        step_size = requested_step_size;
                    }
                    Ok(WorkerRequest::SimContinuous(requested_step_size)) => {
                        continuous = true;
                        step_size = requested_step_size;
                    }
                    Err(_) => {
                        trace!("Worker thread ending (requests channel dropped)");
                        return;
                    }
                }
                let mut record = true;
                loop {
                    let t1 = Instant::now();
                    simulation.step(&step_size);
                    let t2 = Instant::now();
                    if results_tx
                        .send(WorkerResult {
                            result: simulation.clone(),
                            record,
                            time: t2 - t1,
                        })
                        .is_err()
                    {
                        trace!("Worker thread ending (results channel dropped)");
                        return;
                    }
                    if !continuous {
                        break;
                    }
                    record = false;
                }
            }
        });
        Self {
            requests_tx,
            results_rx,
            request_count: 0,
        }
    }
    pub fn request(&mut self, request: WorkerRequest) {
        self.request_count += 1;
        self.requests_tx
            .send(request)
            .expect("Automaton simulation worker thread died unexpectedly")
    }
    pub fn take(&mut self) -> Option<WorkerResult<T>> {
        match self.results_rx.try_recv() {
            Ok(ret) => {
                if self.request_count > 0 {
                    self.request_count -= 1;
                }
                Some(ret)
            }
            Err(mpsc::TryRecvError::Empty) => None,
            Err(mpsc::TryRecvError::Disconnected) => {
                panic!("Automaton simulation worker thread died unexpectedly")
            }
        }
    }
    pub fn get_request_count(&self) -> usize {
        self.request_count
    }
}
