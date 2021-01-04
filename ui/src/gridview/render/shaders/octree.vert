#version 140

in vec2 pos;

out vec2 screen_pos;

uniform mat4 matrix;

void main() {
    gl_Position = vec4(pos, 0.0, 1.0);
    screen_pos = pos;
}
