#version 140

in vec2 pos;

out vec2 screen_pos;

void main() {
    gl_Position = vec4(pos, 0.0, 1.0);
    screen_pos = pos;
}
