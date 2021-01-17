//! GLSL shader programs.

use glium::Program;
use log::error;
use send_wrapper::SendWrapper;

use crate::DISPLAY;

#[macro_use]
mod macros;

mod wrapped;

use wrapped::*;

#[cfg(debug_assertions)]
pub type WrappedShader = DynamicWrappedShader;
#[cfg(not(debug_assertions))]
pub type WrappedShader = StaticWrappedShader;

shader!(RGBA_2D      = { srgb: true,  "rgba_2d" });
shader!(RGBA_3D      = { srgb: true,  "rgba_3d" });

shader!(QUADTREE     = { srgb: true,  vert: "screen_pos", frag: "quadtree" });
shader!(OCTREE       = { srgb: true,  vert: "screen_pos", frag: "octree" });

shader!(GRIDLINES_3D = { srgb: true,  "gridlines_3d" });

shader!(PICKER       = { srgb: false, "picker" });
shader!(PIXMIX       = { srgb: false, "pixmix" });

pub fn hot_reload_all() {
    #[cfg(debug_assertions)]
    {
        log::info!("Reloading shaders");

        RGBA_2D.try_reload();
        RGBA_3D.try_reload();

        QUADTREE.try_reload();
        OCTREE.try_reload();

        GRIDLINES_3D.try_reload();

        PICKER.try_reload();
        PIXMIX.try_reload();
    }

    #[cfg(not(debug_assertions))]
    error!("Cannot reload shaders in this build");
}

fn compile_shader(
    srgb: bool,
    vert_name: &str,
    frag_name: &str,
    vert_contents: &str,
    frag_contents: &str,
) -> Option<Program> {
    glium::program!(
        &**DISPLAY,
        140 => {
            vertex: &preprocess_glsl(vert_contents),
            fragment: &preprocess_glsl(frag_contents),
            outputs_srgb: srgb,
        },
    )
    .map_err(|e| {
        error!(
            "Error compiling {:?}+{:?} shader:\n\n{}",
            vert_name, frag_name, e,
        )
    })
    .ok()
}

// Glium doesn't make it easy to reuse GLSL routines or link extra files into a
// shader program, so instead we preprocess each GLSL file using this function,
// looking for `//@ `<filename>` goes here` and replacing that with the contents
// of that file. This is a dumb hack but not as dumb as duplicating GLSL code in
// multiple places.
fn preprocess_glsl(glsl: &str) -> String {
    let mut ret = String::new();
    for line in glsl.lines() {
        ret.push_str(line);
        ret.push('\n');
        if line.trim().starts_with("//#include ") {
            let file_to_include = match line.trim_start_matches("//#include ") {
                "util/fog.frag" => include_str!("util/fog.frag"),
                "util/lighting.frag" => include_str!("util/lighting.frag"),
                other => panic!(
                    "Invalid `//#include` preprocessor directive in some GLSL file somewhere: {:?}",
                    other,
                ),
            };
            ret.push_str(file_to_include);
        }
    }
    ret
}
