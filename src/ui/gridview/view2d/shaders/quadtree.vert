#version 140

in vec2 texture_pos;
in vec2 cell_pos;

out vec2 CellPos;

void main() {
    gl_Position = vec4(texture_pos, 0.0, 1.0);
    CellPos = cell_pos;
}
