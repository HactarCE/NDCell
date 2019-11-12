pub fn compile(display: &glium::Display) -> glium::Program {
    glium::Program::from_source(
        display,
        VERTEX_SHADER_SRC,
        FRAGMENT_SHADER_SRC,
        Some(GEOMETRY_SHADER_SRC),
    )
    .expect(&format!(
        "Failed to compile shader in {}",
        std::module_path!()
    ))
}

pub const VERTEX_SHADER_SRC: &str = r#"
    #version 140

    in vec2 position;
    in vec4 color;
    out vec4 vColor;

    void main() {
        gl_Position = vec4(position, 0.0, 1.0);
        vColor = color;
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
    uniform float low_offset;
    uniform float high_offset;

    void main() {
        fColor = vColor[0];
        gl_Position = matrix * (gl_in[0].gl_Position + vec4(low_offset, low_offset, 0.0, 0.0));
        EmitVertex();
        gl_Position = matrix * (gl_in[0].gl_Position + vec4(high_offset, low_offset, 0.0, 0.0));
        EmitVertex();
        gl_Position = matrix * (gl_in[0].gl_Position + vec4(low_offset, high_offset, 0.0, 0.0));
        EmitVertex();
        gl_Position = matrix * (gl_in[0].gl_Position + vec4(high_offset, high_offset, 0.0, 0.0));
        EmitVertex();
        gl_Position = matrix * (gl_in[0].gl_Position + vec4(low_offset, low_offset, 0.0, 0.0));
        EmitVertex();
        EndPrimitive();
    }
"#;

pub const FRAGMENT_SHADER_SRC: &str = r#"
    #version 140

    in vec4 fColor;
    out vec4 color;

    void main() {
        // color = vec4(1.0, 0.0, 1.0, 1.0);
        color = fColor;
    }
"#;