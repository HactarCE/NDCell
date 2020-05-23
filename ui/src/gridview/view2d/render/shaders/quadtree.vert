#version 140

in vec2 cell_coords;
in vec2 dest_coords;

out vec2 vCellPos;

void main() {
    gl_Position = vec4(dest_coords, 0.0, 1.0);
    vCellPos = cell_coords;
}
