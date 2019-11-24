//! N-dimensional cellular automaton simulation program

#![allow(dead_code)]
#![warn(missing_docs)]

pub mod automaton;
mod math;
mod ui;

fn main() {
    ui::show_gui()
}
