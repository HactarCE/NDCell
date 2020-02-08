//! N-dimensional cellular automaton simulation program

#![allow(dead_code)]
#![warn(missing_docs)] // TODO: pull this band-aid off.

#[macro_use]
extern crate glium;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate ref_thread_local;

use log::{debug, info};

pub mod automaton;
mod math;
mod ui;

fn main() {
    simple_logger::init().unwrap();
    info!("Starting NDCell v{} ...", env!("CARGO_PKG_VERSION"));

    // The default stack size on Windows is 1 MB, which is not enough for
    // NDCell. The easiest workaround is to just spawn a new thread with a large
    // enough stack (16 MB in this case).
    #[cfg(windows)]
    {
        use std::thread;

        const MSVC_STACK_SIZE_MB: usize = 16;

        debug!("Spawning UI thread {} MB stack size", MSVC_STACK_SIZE_MB);
        let _ = thread::Builder::new()
            .stack_size(MSVC_STACK_SIZE_MB * 1024 * 1024)
            .spawn(ui::show_gui)
            .unwrap()
            .join();
    }

    // On Unix-based systems there is no need to spawn a new thread.
    #[cfg(unix)]
    {
        debug!("Launching UI");
        ui::show_gui();
    }
}
