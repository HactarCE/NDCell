use glium::program;

pub fn compile_cell_program(display: &glium::Display) -> glium::Program {
    program!(
        display,
        140 => {
            vertex: include_str!("cell_chunk.vert"),
            fragment: include_str!("cell_gradient.frag"),
            outputs_srgb: false,
        }
    )
    .expect(&format!(
        "Failed to compile shaders in {} near line {}",
        std::module_path!(),
        std::line!(),
    ))
}

pub fn compile_lines_program(display: &glium::Display) -> glium::Program {
    program!(
        display,
        140 => {
            vertex: include_str!("lines.vert"),
            fragment: include_str!("lines.frag"),
            outputs_srgb: true,
        }
    )
    .expect(&format!(
        "Failed to compile shaders in {} near line {}",
        std::module_path!(),
        std::line!(),
    ))
}
