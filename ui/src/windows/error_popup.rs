use anyhow::Result;
use imgui::{im_str, ImString, Ui};
use parking_lot::Mutex;

use crate::gridview::GridView;

lazy_static! {
    static ref LAST_ERROR: Mutex<Option<String>> = Mutex::new(None);
    static ref CLIPBOARD_ERROR: Mutex<bool> = Mutex::new(false);
    static ref ERROR_POPUP_TITLE: &'static imgui::ImStr = im_str!("An internal error has occurred");
}

pub fn show_error_popup_on_error(
    ui: &Ui<'_>,
    gridview: &GridView,
    gridview_frame_result: Result<()>,
) {
    let mut last_error = LAST_ERROR.lock();
    let mut clipboard_error = CLIPBOARD_ERROR.lock();

    if let Err(e) = gridview_frame_result {
        if last_error.is_none() {
            ui.open_popup(*ERROR_POPUP_TITLE);
            *last_error = Some(format!("Error occurred while: {:?}", e));
        }
    }

    let error_string = last_error.clone().unwrap_or_default();

    unsafe {
        // Workaround for https://github.com/imgui-rs/imgui-rs/issues/201
        imgui::sys::igSetNextWindowSize(
            imgui::sys::ImVec2::new(500.0, 0.0),
            imgui::Condition::Always as i32,
        );
    }

    let mut stayed_open = true;
    ui.popup_modal(*ERROR_POPUP_TITLE)
        .opened(&mut stayed_open)
        .resizable(false)
        .build(|| {
            ui.text_wrapped(im_str!("Yikes! That wasn't supposed to happen."));
            ui.text("");
            if *clipboard_error {
                ui.text_wrapped(im_str!("Oh geez, another error just happened when you clicked that button. You're on your own now."));
            } else {
                ui.text_wrapped(im_str!("Don't worry, your work isn't lost. The 'OK' button should take you right back where you were. But in case the error keeps happening, click the button below to copy everything to the clipboard:"));
                if ui.button(im_str!("Copy CA contents to clipboard"), [0.0, 0.0]) {
                    let fmt = ndcell_core::io::CaFormat::Macrocell;
                    if let Ok(s) = gridview.export(fmt) {
                        if crate::clipboard_compat::clipboard_set(s).is_err() {
                            *clipboard_error = true;
                        }
                    } else {
                        *clipboard_error = true;
                    }
                }
            }
            ui.text("");
            ui.text_wrapped(im_str!("Please report this to the developer, along with the exception info below:"));
            ui.input_text_multiline(im_str!(""), &mut ImString::new(&error_string), [0.0, 0.0])
                .read_only(true)
                .build();
            if ui.button(im_str!("Copy exception info"), [0.0, 0.0]) {
                if crate::clipboard_compat::clipboard_set(error_string).is_err() {
                    *clipboard_error = true;
                }
            }
            ui.text("");

            if ui.button(im_str!("OK"), [0.0, 0.0]) {
                ui.close_current_popup();
            }
        });

    if !stayed_open {
        *last_error = None;
        *clipboard_error = false;
    }
}
