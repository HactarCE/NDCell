use imgui::*;

use crate::config::*;
use crate::gridview::*;

pub fn build(ui: &imgui::Ui, config: &mut Config, gridview: &GridView) {
    let mut menu_bar_height = 0.0;
    if let Some(menu_bar) = ui.begin_main_menu_bar() {
        if let Some(menu) = ui.begin_menu(im_str!("File"), true) {
            MenuItem::new(im_str!("Hello")).build(ui);
            menu.end(ui);
        }
        if let Some(menu) = ui.begin_menu(im_str!("Edit"), true) {
            MenuItem::new(im_str!("Undo"))
                .shortcut(im_str!("CTRL+Z"))
                .build(ui);
            MenuItem::new(im_str!("Redo"))
                .shortcut(im_str!("CTRL+Y"))
                .enabled(false)
                .build(ui);
            ui.separator();
            MenuItem::new(im_str!("Cut"))
                .shortcut(im_str!("CTRL+X"))
                .build(ui);
            MenuItem::new(im_str!("Copy"))
                .shortcut(im_str!("CTRL+C"))
                .build(ui);
            MenuItem::new(im_str!("Paste"))
                .shortcut(im_str!("CTRL+V"))
                .build(ui);
            menu.end(ui);
        }
        if let Some(menu) = ui.begin_menu(im_str!("Windows"), true) {
            menu.end(ui);
        }
        menu_bar_height = ui.window_size()[1];
        menu_bar.end(ui);
    }
    imgui::Window::new(im_str!(" "))
        .bg_alpha(1.0)
        .position([0.0, menu_bar_height], Condition::Once)
        .size([500.0, 500.0], Condition::Once)
        .menu_bar(false)
        .title_bar(false)
        .collapsible(false)
        .resizable(false)
        .movable(false)
        .bring_to_front_on_focus(false)
        .nav_focus(false)
        .build(ui, || {});

    // ui.main_menu_bar(|| {
    //     ui.ui.button(im_str!("Menu 1"), [100.0, 20.0]);
    // });
    // imgui::Dock::new().build(|root| {
    // root
    // .size([500_f32, 500_f32])
    // .position([0_f32, 0_f32])
    // .split(
    //     imgui::Direction::Left,
    //     0.7_f32,
    //     |left| {
    //         // left.dock_window(&imgui::ImString::new(crate::TITLE));
    //     },
    //     |right| {
    //         // right.dock_window(im_str!("Simulation"));
    //     },
    // )
    // });
}
