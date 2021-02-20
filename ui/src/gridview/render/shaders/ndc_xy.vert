#version 150

in vec2 pos;

out vec2 ndc_xy; // normalized device coordinates XY

void main() {
    gl_Position = vec4(pos, 0.0, 1.0);
    ndc_xy = pos;
}
