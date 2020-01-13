#version 140

in vec2 pos;
in vec2 tex_coords;

out vec2 TexCoords;

void main() {
    gl_Position = vec4(pos, 0.0, 1.0);
    TexCoords = tex_coords;
}
