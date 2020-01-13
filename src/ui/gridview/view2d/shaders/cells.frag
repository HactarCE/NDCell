#version 140

in vec2 TexCoords;

out vec4 FragColor;

uniform sampler2D cells_texture;

void main() {
    FragColor = texture(cells_texture, TexCoords);
}
