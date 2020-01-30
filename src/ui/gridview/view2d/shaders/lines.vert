#version 140

in vec2 pos;
in vec4 color;

out vec4 line_color;

uniform mat4 matrix;

void main() {
    gl_Position = matrix * vec4(pos, 0.0, 1.0);
    line_color = color;
}
