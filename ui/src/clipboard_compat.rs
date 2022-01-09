//! Implementation of imgui::ClipboardBackend using the clipboard crate.

use anyhow::{anyhow, Context, Result};
use clipboard::{ClipboardContext, ClipboardProvider};
use log::warn;
use parking_lot::Mutex;

lazy_static! {
    static ref CTX: Mutex<Option<ClipboardContext>> = Mutex::new(ClipboardProvider::new().ok());
}

pub fn clipboard_get() -> Result<String> {
    CTX.lock()
        .as_mut()
        .ok_or(anyhow!("No clipboard provider"))?
        .get_contents()
        .map_err(|e| anyhow!("{}", e.to_string()))
        .context("Getting clipboard contents")
}

pub fn clipboard_set(new_contents: String) -> Result<()> {
    CTX.lock()
        .as_mut()
        .ok_or(anyhow!("No clipboard provider"))?
        .set_contents(new_contents)
        .map_err(|e| anyhow!("{}", e.to_string()))
        .context("Setting clipboard contents")
}

pub struct ClipboardCompat;

impl imgui::ClipboardBackend for ClipboardCompat {
    fn get(&mut self) -> Option<String> {
        clipboard_get().ok()
    }
    fn set(&mut self, value: &str) {
        if clipboard_set(value.to_string()).is_err() {
            warn!("Failed to set clipboard contents");
        }
    }
}
