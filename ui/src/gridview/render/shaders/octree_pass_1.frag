// First pass for sparse voxel octree rendering algorihtm based on Efficient
// Sparse Voxel Octrees by S. Laine, T. Karras.
// (https://users.aalto.fi/~laines9/publications/laine2010i3d_paper.pdf)

#version 150

in vec2 ndc_xy; // -1.0 ... +1.0 (same as `gl_Position`)

out float initial_t_for_pass_2;

uniform float alpha;

// These lines include other files in this GLSL program. See `shaders/mod.rs`.
//#include util/octree.frag

void main() {
    float initial_t = 0.0;
    OctreeRaycastResult result = octree_raycast(initial_t);

    if (result.hit) {
        initial_t_for_pass_2 = result.t;
        TODO TODO TODO
    } else {
        // The ray did not intersect any cell.
        initial_t_for_pass_2 = -1.0;
        TODO TODO TODO
    }
}
