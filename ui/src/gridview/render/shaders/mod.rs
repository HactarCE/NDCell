//! GLSL shader programs.

use glium::Program;
use send_wrapper::SendWrapper;

use crate::DISPLAY;

/// Uses `lazy_static!()` to define a shader that is lazily compiled from files.
macro_rules! static_shader {
    ($name:ident = { $glsl_version:expr, srgb: $srgb:expr, vert: $vert_filename:expr, frag: $frag_filename:expr }) => {
        lazy_static! {
            pub static ref $name: SendWrapper<Program> = SendWrapper::new(
                glium::program!(
                    &**DISPLAY,
                    $glsl_version => {
                        vertex: include_str!(concat!($vert_filename, ".vert")),
                        fragment: include_str!(concat!($frag_filename, ".frag")),
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

static_shader!(PICKER    = { 140, srgb: false, "picker" });
static_shader!(PIXMIX    = { 140, srgb: false, "pixmix" });
static_shader!(QUADTREE  = { 140, srgb: true,  "quadtree" });
static_shader!(RGBA_2D   = { 140, srgb: true,  vert: "rgba_2d", frag: "color" });
static_shader!(RGB_3D    = { 140, srgb: true,  vert: "rgb_3d",  frag: "color" });
