#version 140

in vec3 pos;
in vec3 normal;
in vec4 color;

out vec3 vPos;
out vec4 vColor;

uniform mat4 matrix;

// This line includes another file in this GLSL program. See `shaders/mod.rs`.
//#include lib/lighting.frag

void main() {
    gl_Position = matrix * vec4(pos, 1.0);
    vPos = pos;
    vColor = color;
    vColor.rgb *= compute_lighting(normal);
}
