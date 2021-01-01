#version 140

in vec2 cell_pos;
in vec2 dest_pos;

out vec2 vCellPos;

void main() {
    gl_Position = vec4(dest_pos, 0.0, 1.0);
    vCellPos = cell_pos;
}
