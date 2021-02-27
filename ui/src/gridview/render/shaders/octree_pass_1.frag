// First pass for sparse voxel octree rendering algorihtm based on Efficient
// Sparse Voxel Octrees by S. Laine, T. Karras.
// (https://users.aalto.fi/~laines9/publications/laine2010i3d_paper.pdf)

#version 150

in vec2 ndc_xy; // -1.0 ... +1.0 (same as `gl_Position`)

out float initial_t_for_pass_2;

// Pixel size of the part of the texture being used
uniform uvec2 initial_t_tex_dimensions;

// These lines include other files in this GLSL program. See `shaders/mod.rs`.
//#include util/octree.frag

void main() {
    float initial_t = 0.0;

    vec2 initial_t_tex_max_coords = vec2(initial_t_tex_dimensions) - vec2(1.0);
    vec2 tex_uv = floor(
        (ndc_xy * 0.5 + 0.5) * vec2(initial_t_tex_dimensions)
    ) / vec2(initial_t_tex_dimensions - uvec2(1u));

    // It's better to overestimate node size and get false positives than to
    // miss nodes completely. Decreasing this number makes the optimization more
    // aggressive, but may fail to draw lone cells.
    float first_pass_factor = 4.0;

    OctreeRaycastResult result = octree_raycast(
        (tex_uv * 2.0 - 1.0),
        initial_t,
        first_pass_factor
    );

    if (result.hit) {
        // The ray hit a node small enough non-empty node that some nearby pixel
        // might hit the non-empty portion.
        initial_t_for_pass_2 = result.t;
    } else {
        // The ray did not intersect any node small enough.
        initial_t_for_pass_2 = 1.0 / 0.0; // +inf
    }
}
