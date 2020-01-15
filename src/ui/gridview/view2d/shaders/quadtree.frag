#version 140

in ivec2 CellPos;

// dimensions are 4 by N
uniform isampler2D quadtree_texture;
uniform int max_layer;

out vec4 color;

uvec2 node_count;
void main() {
    node_count = textureSize(quadtree_texture, 0).x;
    int node = 0;
    for (int layer = max_layer - 1; layer >= 0; layer--) {
        bool branch_x = CellPos.x & (1 << layer);
        bool branch_y = CellPos.y & (1 << layer);
        node = getNodeBranch(node, branch_y * 2 + branch_x);
    }
    float r = intBitsToFloat(getNodeBranch(node, 0));
    float g = intBitsToFloat(getNodeBranch(node, 1));
    float b = intBitsToFloat(getNodeBranch(node, 2));
    float a = intBitsToFloat(getNodeBranch(node, 3));
    color = vec4(r, g, b, a);
}

// Returns the value of the given branch of the given node. `node` is the index
// of the node (also the Y position of the row for this node). `branch` is
// the index (also X position) of the branch, which is a int from 0 to 3.
int getNodeBranch(int node, int branch) {
    return texelFetch(quadtree_texture, ivec2(branch, node))
}
