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
