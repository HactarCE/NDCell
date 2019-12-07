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

pub const VERTEX_SHADER_SRC: &str = r#"
    #version 140

    in vec2 position;
    in float proportion_live;

    out vec4 vColor;

    uniform vec2 chunk_pos;
    uniform float chunk_size;
    uniform vec4 color_dead;
    uniform vec4 color_live;

    uniform mat4 matrix;

    void main() {
        float x = mod(gl_InstanceID, chunk_size);
        float y = floor(gl_InstanceID / chunk_size);
        gl_Position = matrix * vec4(chunk_pos * chunk_size + vec2(x, y) + position, 0.0, 1.0);
        float proportion_dead = 1 - proportion_live;
        vColor = color_live * proportion_live + color_dead * proportion_dead;
    }
"#;

pub const FRAGMENT_SHADER_SRC: &str = r#"
    #version 140

    in vec4 vColor;
    out vec4 color;

    void main() {
        color = vColor;
    }
"#;
