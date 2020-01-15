#version 140

in vec2 texture_pos;
in ivec2 cell_pos;

out ivec2 CellPos;

void main() {
    gl_Position = texture_pos;
    CellPos = cell_pos;
}
