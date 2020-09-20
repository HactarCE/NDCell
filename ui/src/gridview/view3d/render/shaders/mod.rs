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

static_shader!(POINTS   = { "points",   140, srgb: true  });
