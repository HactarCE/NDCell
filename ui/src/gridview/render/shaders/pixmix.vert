#version 150

in vec2 src_coords;
in vec2 dest_coords;

out vec2 tex_coords;

void main() {
    gl_Position = vec4(dest_coords, 0.0, 1.0);
    tex_coords = src_coords;
}
