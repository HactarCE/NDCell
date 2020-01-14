//! N-dimensional cellular automaton simulation program

#![allow(dead_code)]
#![warn(missing_docs)]

#[macro_use]
extern crate pest_derive;

pub mod automaton;
mod math;
mod ui;

fn main() {
    simple_logger::init().unwrap();
    ui::show_gui()
}
