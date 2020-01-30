#version 140

in vec2 tex_coords;

out vec4 color;

uniform sampler2D src_texture;
uniform float alpha;

void main() {
    color = texture(src_texture, tex_coords).rgba;
    // Multiply alpha by the uniform value.
    color.a *= alpha;
}
