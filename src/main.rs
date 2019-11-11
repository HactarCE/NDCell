//! N-dimensional cellular automaton simulation program

#![warn(missing_docs)]
#![allow(dead_code)]

pub mod automaton;
mod ui;

fn main() {
    ui::show_gui()
}
