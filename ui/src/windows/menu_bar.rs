use imgui::im_str;

use super::BuildParams;

pub fn build(params: &mut BuildParams<'_>) {
    let BuildParams { ui, gridview, .. } = params;

    ui.main_menu_bar(|| {
        ui.menu(im_str!("File"), true, || {
            if imgui::MenuItem::new(im_str!("New")).build(ui) {
                todo!("New");
            }
            if imgui::MenuItem::new(im_str!("Open")).build(ui) {
                todo!("Open");
            }
            if imgui::MenuItem::new(im_str!("Save RLE")).build(ui) {
                todo!("Save RLE");
            }
            if imgui::MenuItem::new(im_str!("Save Macrocell")).build(ui) {
                todo!("Save Macrocell");
            }
            if imgui::MenuItem::new(im_str!("Quit")).build(ui) {
                todo!("Quit");
            }
        });

        ui.menu(im_str!("Edit"), true, || {
            if imgui::MenuItem::new(im_str!("Undo")).build(ui) {
                todo!("Undo");
            }
            if imgui::MenuItem::new(im_str!("Redo")).build(ui) {
                todo!("Redo");
            }
            ui.separator();
            let has_sel = gridview.has_selection();
            if imgui::MenuItem::new(im_str!("Cut"))
                .enabled(has_sel)
                .build(ui)
            {
                todo!("Cut");
            }
            if imgui::MenuItem::new(im_str!("Copy RLE"))
                .enabled(has_sel)
                .build(ui)
            {
                todo!("Copy RLE");
            }
            if imgui::MenuItem::new(im_str!("Copy Macrocell"))
                .enabled(has_sel)
                .build(ui)
            {
                todo!("Copy Macrocell");
            }
            if imgui::MenuItem::new(im_str!("Paste"))
                .enabled(has_sel)
                .build(ui)
            {
                todo!("Paste");
            }
            if imgui::MenuItem::new(im_str!("Erase"))
                .enabled(has_sel)
                .build(ui)
            {
                todo!("Erase");
            }
            ui.separator();
            if imgui::MenuItem::new(im_str!("Select All")).build(ui) {
                todo!("Select All");
            }
            if imgui::MenuItem::new(im_str!("Deselect"))
                .enabled(has_sel)
                .build(ui)
            {
                todo!("Deselect");
            }
        });

        ui.menu(im_str!("Simulation"), true, || {
            if imgui::MenuItem::new(im_str!(
                "Run continuously <TODO change this text depending on sim state>"
            ))
            .build(ui)
            {
                todo!("Run continuously <TODO change this text depending on sim state>");
            }
            if imgui::MenuItem::new(im_str!("Step 1")).build(ui) {
                todo!("Step 1");
            }
            if imgui::MenuItem::new(im_str!("Step <TODO STEP SIZE GOES HERE>")).build(ui) {
                todo!("Step <TODO STEP SIZE GOES HERE>");
            }
            ui.separator();
            if imgui::MenuItem::new(im_str!("Reset")).build(ui) {
                todo!("Reset");
            }
            if imgui::MenuItem::new(im_str!("Set generation...")).build(ui) {
                todo!("Set generation...");
            }
            ui.separator();
            if imgui::MenuItem::new(im_str!("Faster")).build(ui) {
                todo!("Faster");
            }
            if imgui::MenuItem::new(im_str!("Slower")).build(ui) {
                todo!("Slower");
            }
            if imgui::MenuItem::new(im_str!("Set step size...")).build(ui) {
                todo!("Set step size...");
            }
        });

        ui.menu(im_str!("View"), true, || {
            if imgui::MenuItem::new(im_str!("Fit pattern")).build(ui) {
                todo!("Fit pattern");
            }
            if imgui::MenuItem::new(im_str!("Fit selection")).build(ui) {
                todo!("Fit selection");
            }
            if imgui::MenuItem::new(im_str!("Middle")).build(ui) {
                todo!("Middle");
            }
            ui.separator();
            if imgui::MenuItem::new(im_str!("Zoom in")).build(ui) {
                todo!("Zoom in");
            }
            if imgui::MenuItem::new(im_str!("Zoom out")).build(ui) {
                todo!("Zoom out");
            }
            if imgui::MenuItem::new(im_str!("Set scale...")).build(ui) {
                todo!("Set scale...");
            }
        });

        ui.menu(im_str!("Setup"), true, || {
            if imgui::MenuItem::new(im_str!("Load rule from clipboard")).build(ui) {
                todo!("Load rule from clipboard");
            }
            ui.separator();
            if imgui::MenuItem::new(im_str!("Load cell pattern from clipboard")).build(ui) {
                todo!("Load cell pattern from clipboard");
            }
            ui.separator();
            if imgui::MenuItem::new(im_str!(
                "Load colors from clipboard\n(one CSS color per line)"
            ))
            .build(ui)
            {
                todo!("Load colors from clipboard\n(one CSS color per line)");
            }
            if imgui::MenuItem::new(im_str!("Reset colors")).build(ui) {
                todo!("Reset colors");
            }
        });

        ui.menu(im_str!("Windows"), true, || {
            let mut simulation_window: bool = false;
            ui.checkbox(im_str!("Simulation"), &mut simulation_window);

            let mut debug_window: bool = false;
            #[cfg(debug_assertions)]
            ui.checkbox(im_str!("Debug values"), &mut debug_window);
        });
    });
}
