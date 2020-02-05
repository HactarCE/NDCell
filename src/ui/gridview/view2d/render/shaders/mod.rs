use glium::Program;
use ref_thread_local::{Ref, RefThreadLocal};

macro_rules! load_shader {
    ($name:expr, $version:expr, srgb = $srgb:expr) => {
        {
            let display = &*crate::ui::get_display();
            glium::program!(
                display,
                $version => {
                    vertex: include_str!(concat!(stringify!($name), ".vert")),
                    fragment: include_str!(concat!(stringify!($name), ".frag")),
                    outputs_srgb: $srgb,
                },
            ).expect(&format!("Failed to compile '{}' shader in {}", stringify!($name), std::module_path!()))
        }
    };
}

ref_thread_local! {
    static managed BLIT: Program = load_shader!(blit, 140, srgb = false);
    static managed LINES: Program = load_shader!(lines, 140, srgb = true);
    static managed QUADTREE: Program = load_shader!(quadtree, 140, srgb = true);
}

pub fn blit<'a>() -> Ref<'a, Program> {
    BLIT.borrow()
}
pub fn lines<'a>() -> Ref<'a, Program> {
    LINES.borrow()
}
pub fn quadtree<'a>() -> Ref<'a, Program> {
    QUADTREE.borrow()
}
