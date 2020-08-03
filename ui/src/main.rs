//! The graphical frontend.
//!
//! This module contains everything needed to display NDCell's UI.

#![allow(dead_code)]
#![warn(missing_docs)]

#[macro_use]
extern crate glium;
#[macro_use]
extern crate lazy_static;

use log::{debug, info};

mod clipboard_compat;
mod config;
mod gridview;
mod gui;
mod history;
mod input;
mod windows;

use gui::DISPLAY;

/// The title of the window (both the OS window, and the main imgui window).
const TITLE: &str = "NDCell";

fn main() {
    simple_logger::init().unwrap();
    info!("Starting NDCell v{} ...", env!("CARGO_PKG_VERSION"));
    debug!("Launching UI");
    gui::show_gui();
}

fn load_custom_rule() -> ndcell_core::Simulation<ndcell_core::Dim2D> {
    use ndcell_core::*;
    use std::fs::File;
    use std::io::Read;
    use std::sync::Arc;

    File::open("rule.ndca")
        .map(|mut file| {
            let mut source_code = String::new();
            file.read_to_string(&mut source_code)
                .expect("Error reading file");
            Simulation::from(
                ndcell_lang::compile_blocking(Arc::new(source_code), None)
                    .expect("Error compiling rule"),
            )
        })
        .unwrap_or_else(|_| Simulation::from(rule::LIFE))
}
