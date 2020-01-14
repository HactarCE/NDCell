//! Implementation of imgui::ClipboardBackend using the clipboard crate.

use clipboard;
use clipboard::{ClipboardContext, ClipboardProvider};
use imgui;
use log::warn;
use std::cell::RefCell;

thread_local!(
    static CTX: RefCell<Option<ClipboardContext>> = RefCell::new(ClipboardProvider::new().ok()) ) ;

pub fn clipboard_get() -> Result<String, ()> {
    CTX.with(|ctx| {
        ctx.borrow_mut()
            .as_mut()
            .ok_or(())
            .and_then(|mut ctx| ctx.get_contents().map_err(|_| ()))
    })
}

pub fn clipboard_set(new_contents: String) -> Result<(), ()> {
    CTX.with(|ctx| {
        ctx.borrow_mut()
            .as_mut()
            .ok_or(())
            .and_then(|mut ctx| ctx.set_contents(new_contents).map_err(|_| ()))
    })
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
