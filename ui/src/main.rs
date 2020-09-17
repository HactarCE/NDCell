//! The graphical frontend.
//!
//! This module contains everything needed to display NDCell's UI.

#![allow(dead_code)]
#![warn(missing_docs)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[macro_use]
extern crate glium;
#[macro_use]
extern crate lazy_static;

use log::{debug, info};

use ndcell_core::dim::Dim2D;
use ndcell_core::sim::HashLife;

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

fn load_custom_rule() -> HashLife<Dim2D> {
    use std::fs::File;
    use std::io::Read;
    use std::sync::Arc;

    File::open("rule.ndca")
        .map(|mut file| {
            let mut source_code = String::new();
            file.read_to_string(&mut source_code)
                .expect("Error reading file");
            HashLife::from(
                ndcell_lang::compile_blocking(Arc::new(source_code), None)
                    .expect("Error compiling rule"),
            )
        })
        .unwrap_or_else(|_| HashLife::from(ndcell_core::sim::rule::LIFE))
}
