//! Implementation of imgui::ClipboardBackend using the clipboard crate.

use clipboard;
use clipboard::{ClipboardContext, ClipboardProvider};
use imgui;
use log::warn;
use std::sync::Mutex;

lazy_static! {
    static ref CTX: Mutex<Option<ClipboardContext>> = Mutex::new(ClipboardProvider::new().ok());
}

pub fn clipboard_get() -> Result<String, ()> {
    CTX.lock()
        .unwrap()
        .as_mut()
        .ok_or(())
        .and_then(|ctx| ctx.get_contents().map_err(|_| ()))
}

pub fn clipboard_set(new_contents: String) -> Result<(), ()> {
    CTX.lock()
        .unwrap()
        .as_mut()
        .ok_or(())
        .and_then(|ctx| ctx.set_contents(new_contents).map_err(|_| ()))
}

pub struct ClipboardCompat;

impl imgui::ClipboardBackend for ClipboardCompat {
    fn get(&mut self) -> Option<imgui::ImString> {
        clipboard_get().ok().map(imgui::ImString::new)
    }
    fn set(&mut self, value: &imgui::ImStr) {
        if clipboard_set(value.to_string()).is_err() {
            warn!("Failed to set clipboard contents");
        };
    }
}
