use anyhow::{anyhow, bail, Context, Result};
use imgui::*;
use palette::Srgba;
use std::convert::TryInto;
use std::sync::Arc;

use ndcell_core::traits::*;

use crate::gridview::{GridView, GridView2D, GridView3D};
use crate::windows::BuildParams;
use crate::CONFIG;

#[derive(Debug, Default)]
pub struct SetupWindow {
    pub is_visible: bool,
    pub error_message: Option<String>,
}
impl SetupWindow {
    fn error(&mut self, ui: &Ui<'_>, error: anyhow::Error) {
        self.error_message = Some(format!("{:?}", error));
        ui.open_popup("Error");
    }

    fn load_rule_from_clipboard(gridview: &mut GridView) -> Result<()> {
        let source_code = Arc::new(crate::clipboard_compat::clipboard_get()?);
        bail!("custom rules are not yet fully implemented");
        // let rule = ndcell_lang::compile_blocking(Arc::clone(&source_code), None)
        //     .map_err(|e| e.with_source(&source_code))
        //     .context("Compiling rule")?;
        // println!("{:?}", rule.rule_meta());
        // if rule.rule_meta().ndim != gridview.ndim() {
        //     *gridview = match rule.rule_meta().ndim {
        //         2 => GridView2D::default().into(),
        //         3 => GridView3D::default().into(),
        //         d => bail!("Cannot display {}D simulation", d),
        //     }
        // }
        // match gridview {
        //     GridView::View2D(gv2d) => gv2d.automaton.rule = rule.into_arc(),
        //     GridView::View3D(gv3d) => gv3d.automaton.rule = rule.into_arc(),
        // }

        Ok(())
    }
    fn load_cell_pattern_from_clipboard(gridview: &mut GridView) -> Result<()> {
        use ndcell_core::io::import_automaton_from_string;
        let s = crate::clipboard_compat::clipboard_get()?;

        match gridview {
            GridView::View2D(gv2d) => {
                gv2d.automaton = import_automaton_from_string(&s, Arc::clone(&gv2d.automaton.rule))
                    .map_err(|es| anyhow!("{}", itertools::join(es, "\n")))
                    .context("Importing cell pattern")?
                    .unwrap()
                    .try_into()
                    .unwrap();
            }
            GridView::View3D(gv3d) => {
                gv3d.automaton = import_automaton_from_string(&s, Arc::clone(&gv3d.automaton.rule))
                    .map_err(|es| anyhow!("{}", itertools::join(es, "\n")))
                    .context("Importing cell pattern")?
                    .unwrap()
                    .try_into()
                    .unwrap();
            }
        }

        Ok(())
    }
    fn load_colors_from_clipboard() -> Result<()> {
        let mut cfg = CONFIG.lock();
        let colors = &mut cfg.gfx.cell_colors;

        let s = crate::clipboard_compat::clipboard_get()?;
        let mut i = 1;
        for line in s.lines() {
            if !line.is_empty() {
                let c: css_color_parser::Color = line
                    .parse()
                    .context(format!("Unable to parse color: {:?}", line))?;
                colors[i] = Srgba::new(
                    c.r as f32 / 255.0,
                    c.g as f32 / 255.0,
                    c.b as f32 / 255.0,
                    c.a,
                );
                i += 1;
                if i > colors.len() {
                    break;
                }
            }
        }
        crate::gridview::render::invalidate_gl_ndtree_cache();

        Ok(())
    }

    /// Builds the window.
    pub fn build(&mut self, params: &mut BuildParams<'_>) {
        let BuildParams { ui, gridview, .. } = params;

        if self.is_visible {
            Window::new("Setup")
                .size([300.0, 0.0], Condition::FirstUseEver)
                .flags(WindowFlags::NO_RESIZE)
                .build(ui, || {
                    if ui.button_with_size(
                        "Load rule from clipboard",
                        [ui.window_content_region_width(), 60.0],
                    ) {
                        if let Err(e) = Self::load_rule_from_clipboard(gridview) {
                            self.error(ui, e);
                        }
                    }

                    if ui.button_with_size(
                        "Load cell pattern from clipboard",
                        [ui.window_content_region_width(), 60.0],
                    ) {
                        if let Err(e) = Self::load_cell_pattern_from_clipboard(gridview) {
                            self.error(ui, e);
                        }
                    }

                    if ui.button_with_size(
                        "Load colors from clipboard\n(one color per line, CSS color format)",
                        [ui.window_content_region_width(), 60.0],
                    ) {
                        if let Err(e) = Self::load_colors_from_clipboard() {
                            self.error(ui, e);
                        }
                    }

                    if ui.button_with_size("Reset colors", [ui.window_content_region_width(), 30.0])
                    {
                        CONFIG.lock().gfx.cell_colors = crate::default_colors();
                    }

                    PopupModal::new("Error")
                        .flags(WindowFlags::NO_RESIZE | WindowFlags::NO_MOVE)
                        .build(ui, || {
                            if let Some(e) = &self.error_message {
                                ui.text(e);
                                ui.text("");
                                if ui
                                    .button_with_size("OK", [ui.window_content_region_width(), 0.0])
                                {
                                    self.error_message = None;
                                    ui.close_current_popup();
                                }
                            } else {
                                ui.close_current_popup();
                            }
                        });
                });
        }
    }
}
