use glium::program;

pub fn compile(display: &glium::Display) -> glium::Program {
    program!(
        display,
        140 => {
            vertex: VERTEX_SHADER_SRC,
            fragment: FRAGMENT_SHADER_SRC,
            outputs_srgb: true,
        }
    )
    .expect(&format!(
        "Failed to compile shader in {}",
        std::module_path!()
    ))
}

const VERTEX_SHADER_SRC: &str = r#"
    #version 140

    in vec2 position;

    uniform mat4 matrix;

    void main() {
        gl_Position = matrix * vec4(position, 0.0, 1.0);
    }
"#;

const FRAGMENT_SHADER_SRC: &str = r#"
    #version 140

    out vec4 color;

    uniform vec4 grid_color;

    void main() {
        color = grid_color;
    }
"#;
