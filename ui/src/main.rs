//! The graphical frontend.
//!
//! This module contains everything needed to display NDCell's UI.

#![allow(dead_code)] // TODO: remove this line
#![warn(missing_docs)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[macro_use]
extern crate glium;
#[macro_use]
extern crate lazy_static;

use log::{debug, info};
use std::sync::Arc;

use ndcell_core::prelude::{Dim2D, Rule};

mod clipboard_compat;
mod commands;
mod config;
mod gridview;
mod gui;
mod input;
mod scale;
mod windows;

use gui::DISPLAY;
use scale::Scale;

/// The title of the window (both the OS window, and the main imgui window).
const TITLE: &str = "NDCell";

fn main() {
    simple_logger::init().unwrap();
    info!("Starting NDCell v{} ...", env!("CARGO_PKG_VERSION"));
    debug!("Launching UI");
    gui::show_gui();
}

fn load_custom_rule() -> Arc<dyn Rule<Dim2D>> {
    use std::fs::File;
    use std::io::Read;

    File::open("rule.ndca")
        .map(|mut file| -> Arc<dyn Rule<Dim2D>> {
            let mut source_code = String::new();
            file.read_to_string(&mut source_code)
                .expect("Error reading file");
            Arc::new(
                ndcell_lang::compile_blocking(Arc::new(source_code), None)
                    .expect("Error compiling rule"),
            )
        })
        .unwrap_or_else(|_| Arc::new(ndcell_core::sim::rule::LIFE))
}
