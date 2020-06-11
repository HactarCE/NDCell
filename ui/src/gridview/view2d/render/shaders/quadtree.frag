#version 140

in vec2 vCellPos;

// One "pixel" per node; each pixel contains four uints.
uniform usampler2D quadtree_texture;
uniform int max_layer;
uniform uint root_idx;

out vec4 color;

uint texture_width = uint(textureSize(quadtree_texture, 0).x);

// Returns the value of the given branch of the given node. `node` is the index
// of the node (also the Y position of the row for this node). `branch` is
// the index (also X position) of the branch, which is an int from 0 to 3.
uint getNodeBranch(uint node, bool branch_x, bool branch_y) {
    uvec2 texel_pos = uvec2(node % texture_width, node / texture_width);
    uvec4 branches = texelFetch(quadtree_texture, ivec2(texel_pos), 0);
    return branches[(uint(branch_y) << 1) | uint(branch_x)];
}

void main() {
    int cell_x = int(round(vCellPos.x));
    int cell_y = int(round(vCellPos.y));
    uint node = root_idx;
    for (int layer = max_layer - 1; layer >= 0; layer--) {
        bool branch_x = bool(cell_x & (1 << layer));
        bool branch_y = bool(cell_y & (1 << layer));
        node = getNodeBranch(node, branch_x, branch_y);
    }
    // RGBA, from MSB to LSB; convert from 0-255 to 0.0-1.0
    float r = float((node >> 24) & 255u) / 255.0;
    float g = float((node >> 16) & 255u) / 255.0;
    float b = float((node >>  8) & 255u) / 255.0;
    float a = float( node        & 255u) / 255.0;
    color = vec4(r, g, b, a);
}
