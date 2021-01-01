#version 140

in vec3 vPos;
in vec4 vColor;

out vec4 color;

// This line includes another file in this GLSL program. See `shaders/mod.rs`.
//#include lib/fog.frag

void main() {
    color = foggify_color(vPos, vColor);
}
