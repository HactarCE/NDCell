#version 140

in vec2 screen_pos; // 0.0 ... 1.0

out vec4 color;

// One "render cell" per node; each pixel contains four uints.
uniform usampler2D quadtree_texture;
uniform int layer_count;
uniform uint root_idx;

uniform ivec2 offset_into_quadtree;

uint texture_width = uint(textureSize(quadtree_texture, 0).x);

// Returns the value of the given child of the given node. `node` is the index
// of the node; `which_child` describes the position of the child along the X
// and Y axes.
uint getNodeChild(uint node, bvec2 which_child) {
    uint texel_index = node;
    ivec2 texel_pos = ivec2(
        texel_index % texture_width,
        texel_index / texture_width
    );

    uint child_index = uint(which_child.x)
                     | uint(which_child.y) << 1;

    uvec4 children = texelFetch(quadtree_texture, texel_pos, 0);
    return children[child_index];
}

void main() {
    ivec2 cell_pos = ivec2(floor(gl_FragCoord.xy)) + offset_into_quadtree;
    uint node = root_idx;
    for (int child_layer = layer_count - 1; child_layer >= 0; child_layer--) {
        bvec2 which_child = bvec2(cell_pos & (1 << child_layer));
        node = getNodeChild(node, which_child);
    }
    // RGBA, little-endian; convert from 0-255 to 0.0-1.0
    float r = float( node        & 255u) / 255.0;
    float g = float((node >>  8) & 255u) / 255.0;
    float b = float((node >> 16) & 255u) / 255.0;
    float a = float((node >> 24) & 255u) / 255.0;
    color = vec4(r, g, b, a);
}
