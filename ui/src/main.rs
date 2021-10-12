//! The graphical frontend.
//!
//! This module contains everything needed to display NDCell's UI.

#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]
#![allow(dead_code)] // TODO: remove this line
#![warn(missing_docs)]
#![warn(rust_2018_idioms)]
#![warn(clippy::all)]
#![deny(clippy::correctness)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[macro_use]
extern crate glium;
#[macro_use]
extern crate lazy_static;

use log::{debug, info, warn};
use parking_lot::Mutex;
use std::sync::Arc;

use ndcell_core::prelude::*;

#[macro_use]
mod debug;
mod clipboard_compat;
mod colors;
mod commands;
mod config;
mod direction;
mod ext;
mod face;
mod gridview;
mod gui;
mod input;
mod mouse;
mod plane;
mod scale;
mod windows;

use config::Config;
use direction::{Direction, DIRECTIONS};
use face::{Face, FACES};
use gui::DISPLAY;
use plane::Plane;
use scale::Scale;

/// The title of the window (both the OS window, and the main imgui window).
const TITLE: &str = "NDCell";

lazy_static! {
    static ref CONFIG: Mutex<Config> = Mutex::new(Config::default());
}

fn main() {
    simple_logger::SimpleLogger::new()
        .init()
        .expect("Failed to initialize logging");
    info!("Starting NDCell v{} ...", env!("CARGO_PKG_VERSION"));
    debug!("Launching UI");
    gui::show_gui();
}

fn load_custom_rule_2d() -> Rule2D {
    use std::fs::File;
    use std::io::Read;

    File::open("rule.ndca")
        .map(|mut file| -> Rule2D {
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
fn load_custom_rule_3d() -> Rule3D {
    use std::fs::File;
    use std::io::Read;

    File::open("rule.ndca")
        .map(|mut file| -> Rule3D {
            let mut source_code = String::new();
            file.read_to_string(&mut source_code)
                .expect("Error reading file");
            Arc::new(
                ndcell_lang::compile_blocking(Arc::new(source_code), None)
                    .expect("Error compiling rule"),
            )
        })
        .unwrap_or_else(|_| Arc::new(ndcell_core::sim::rule::DummyRule))
}

const DEFAULT_NDIM: usize = 3;
const GOSPER_GLIDER_GUN_SYNTH_RLE: &str = "
#CXRLE Gen=-31
x = 47, y = 14, rule = Life
16bo30b$16bobo16bo11b$16b2o17bobo9b$obo10bo21b2o10b$b2o11b2o31b$bo11b
2o32b3$10b2o20b2o13b$11b2o19bobo9b3o$10bo21bo11bo2b$27bo17bob$27b2o18b
$26bobo!
";
fn make_default_gridview(ndim: usize) -> gridview::GridView {
    match ndim {
        2 => Rle::from_string_to_ndautomaton(
            GOSPER_GLIDER_GUN_SYNTH_RLE,
            crate::load_custom_rule_2d().into(),
        )
        .unwrap_or_else(|_| {
            warn!("Failed to load default pattern; using empty pattern instead");
            Default::default()
        })
        .into(),
        3 => Rle::from_string_to_ndautomaton(
            GOSPER_GLIDER_GUN_SYNTH_RLE,
            crate::load_custom_rule_3d().into(),
        )
        .unwrap_or_else(|_| {
            warn!("Failed to load default pattern; using empty pattern instead");
            Default::default()
        })
        .into(),
        _ => panic!("invalid number of dimensions passed to make_default_gridview()"),
    }
}

fn default_colors() -> [palette::Srgba; 256] {
    use palette::Srgba;
    let mut ret = [Srgba::default(); 256];

    ret[0] = crate::colors::cells::DEAD;
    ret[1] = crate::colors::cells::LIVE;

    for i in 2..256 {
        let c = colorous::SPECTRAL.eval_rational(i as usize - 2, 255);
        ret[i] = Srgba::new(c.r, c.g, c.b, u8::MAX).into_format();
    }

    ret
}
