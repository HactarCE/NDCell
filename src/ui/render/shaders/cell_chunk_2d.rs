use glium::program;

pub fn compile(display: &glium::Display) -> glium::Program {
    program!(
        display,
        140 => {
            vertex: VERTEX_SHADER_SRC,
            geometry: GEOMETRY_SHADER_SRC,
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

    in float proportion_live;
    out vec4 vColor;

    uniform vec2 chunk_pos;
    uniform float chunk_size;
    uniform vec4 color_dead;
    uniform vec4 color_live;

    void main() {
        float x = mod(gl_VertexID, chunk_size);
        float y = floor(gl_VertexID / chunk_size);
        gl_Position = vec4(chunk_pos * chunk_size + vec2(x, y), 0.0, 1.0);
        float proportion_dead = 1 - proportion_live;
        vColor = color_live * proportion_live + color_dead * proportion_dead;
    }
"#;

pub const GEOMETRY_SHADER_SRC: &str = r#"
    #version 150

    layout(points) in;
    layout(triangle_strip, max_vertices = 5) out;

    in vec4 vColor[];
    out vec4 fColor;

    in gl_PerVertex
    {
        vec4 gl_Position;
        float gl_PointSize;
        float gl_ClipDistance[];
    } gl_in[];

    uniform mat4 matrix;

    void main() {
        fColor = vColor[0];
        gl_Position = matrix * (gl_in[0].gl_Position + vec4(0.0, 0.0, 0.0, 0.0));
        EmitVertex();
        gl_Position = matrix * (gl_in[0].gl_Position + vec4(1.0, 0.0, 0.0, 0.0));
        EmitVertex();
        gl_Position = matrix * (gl_in[0].gl_Position + vec4(0.0, 1.0, 0.0, 0.0));
        EmitVertex();
        gl_Position = matrix * (gl_in[0].gl_Position + vec4(1.0, 1.0, 0.0, 0.0));
        EmitVertex();
        gl_Position = matrix * (gl_in[0].gl_Position + vec4(0.0, 0.0, 0.0, 0.0));
        EmitVertex();
        EndPrimitive();
    }
"#;

pub const FRAGMENT_SHADER_SRC: &str = r#"
    #version 140

    in vec4 fColor;
    out vec4 color;

    void main() {
        color = fColor;
    }
"#;
