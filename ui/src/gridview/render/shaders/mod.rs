//! GLSL shader programs.

use glium::Program;
use send_wrapper::SendWrapper;

use crate::DISPLAY;

/// Uses lazy_static!() to define a shader that is lazily loaded from the given
/// filename.
macro_rules! static_shader {
    ($name:ident = { $filename:expr, $glsl_version:expr, srgb: $srgb:expr }) => {
        lazy_static! {
            pub static ref $name: SendWrapper<Program> = SendWrapper::new(
                glium::program!(
                    &**DISPLAY,
                    $glsl_version => {
                        vertex: include_str!(concat!($filename, ".vert")),
                        fragment: include_str!(concat!($filename, ".frag")),
                        outputs_srgb: $srgb,
                    },
                ).expect(&format!("Failed to compile '{}' shader in {}", $filename, std::module_path!()))
            );
        }
    };
}

static_shader!(PICKER    = { "picker",    140, srgb: false  });
static_shader!(PIXMIX    = { "pixmix",    140, srgb: false });
static_shader!(QUADTREE  = { "quadtree",  140, srgb: true  });
static_shader!(RGBA      = { "rgba",      140, srgb: true  });
static_shader!(RGB3D     = { "rgb3d",     140, srgb: true  });
