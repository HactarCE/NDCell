//! GLSL shader programs.

use glium::Program;
use send_wrapper::SendWrapper;

use crate::DISPLAY;

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
                "util/lighting.frag" => include_str!("util/lighting.frag"),
                "util/fog.frag" => include_str!("util/fog.frag"),
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

/// Uses `lazy_static!()` to define a shader that is lazily compiled from files.
macro_rules! static_shader {
    ($name:ident = { $glsl_version:expr, srgb: $srgb:expr, vert: $vert_filename:expr, frag: $frag_filename:expr }) => {
        lazy_static! {
            pub static ref $name: SendWrapper<Program> = SendWrapper::new(
                glium::program!(
                    &**DISPLAY,
                    $glsl_version => {
                        vertex: &preprocess_glsl(include_str!(concat!($vert_filename, ".vert"))),
                        fragment: &preprocess_glsl(include_str!(concat!($frag_filename, ".frag"))),
                        outputs_srgb: $srgb,
                    },
                ).expect(&format!(
                    "Failed to compile '{}'+'{}' shader in {}",
                    $vert_filename,
                    $frag_filename,
                    std::module_path!(),
                ))
            );
        }
    };
    ($name:ident = { $glsl_version:expr, srgb: $srgb:expr, $filename:expr }) => {
        static_shader!($name = { $glsl_version, srgb: $srgb, vert: $filename, frag: $filename });
    };
}

static_shader!(OCTREE      = { 140, srgb: true,  "octree" });
static_shader!(PICKER      = { 140, srgb: false, "picker" });
static_shader!(PIXMIX      = { 140, srgb: false, "pixmix" });
static_shader!(QUADTREE    = { 140, srgb: true,  "quadtree" });
static_shader!(RGBA_2D     = { 140, srgb: true,  vert: "rgba_2d", frag: "color" });
static_shader!(RGBA_3D     = { 140, srgb: true,  vert: "rgba_3d", frag: "color" });
static_shader!(RGBA_2D_FOG = { 140, srgb: true,  vert: "rgba_2d", frag: "fog" });
static_shader!(RGBA_3D_FOG = { 140, srgb: true,  vert: "rgba_3d", frag: "fog" });
