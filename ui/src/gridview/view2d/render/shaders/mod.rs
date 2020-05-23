use crate::DISPLAY;
use glium::Program;
use send_wrapper::SendWrapper;

macro_rules! load_shader {
    ($name:expr, $version:expr, srgb = $srgb:expr) => {
        {
            SendWrapper::new(glium::program!(
                    &**DISPLAY,
                    $version => {
                        vertex: include_str!(concat!(stringify!($name), ".vert")),
                        fragment: include_str!(concat!(stringify!($name), ".frag")),
                        outputs_srgb: $srgb,
                    },
                ).expect(&format!("Failed to compile '{}' shader in {}", stringify!($name), std::module_path!()))
            )
        }
    };
}

lazy_static! {
    pub static ref BLIT: SendWrapper<Program> = load_shader!(blit, 140, srgb = false);
    pub static ref LINES: SendWrapper<Program> = load_shader!(lines, 140, srgb = true);
    pub static ref QUADTREE: SendWrapper<Program> = load_shader!(quadtree, 140, srgb = true);
}
