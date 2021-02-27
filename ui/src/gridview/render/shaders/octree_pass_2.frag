// Second pass for sparse voxel octree rendering algorihtm based on Efficient
// Sparse Voxel Octrees by S. Laine, T. Karras.
// (https://users.aalto.fi/~laines9/publications/laine2010i3d_paper.pdf)

#version 150

const bool IGNORE_DEPTH_ESTIMATE = false;
const bool OVERLAY_DEPTH_ESTIMATE = false;
const bool OVERLAY_ITERATIONS = false;

in vec2 ndc_xy; // -1.0 ... +1.0 (same as `gl_Position`)

out vec4 color;

uniform float alpha;

uniform sampler2D initial_t_texture;
// Pixel size of the texture
vec2 initial_t_tex_size = textureSize(initial_t_texture, 0);
// Pixel size of the part of the texture being used
uniform uvec2 initial_t_tex_dimensions;

// These lines include other files in this GLSL program. See `shaders/mod.rs`.
//#include util/fog.frag
//#include util/lighting.frag
//#include util/octree.frag

void main() {
    color = vec4(0.0);  // Assume transparent.
    gl_FragDepth = 1.0; // Assume far plane.

    // Sample the four nearest points on the `initial_t_texture` and use the
    // minimum one (i.e. start closest to the camera).
    vec2 sample_uv = (ndc_xy * 0.5 + 0.5) * vec2(initial_t_tex_dimensions - 1u);
    ivec2 sample_uv_floor = ivec2(floor(sample_uv));
    float t_lower_bound = min(min(
        texelFetch(initial_t_texture, sample_uv_floor + ivec2(0, 0), 0)[0],
        texelFetch(initial_t_texture, sample_uv_floor + ivec2(0, 1), 0)[0]
    ), min(
        texelFetch(initial_t_texture, sample_uv_floor + ivec2(1, 0), 0)[0],
        texelFetch(initial_t_texture, sample_uv_floor + ivec2(1, 1), 0)[0]
    ));

    float initial_t = t_lower_bound;
    if (IGNORE_DEPTH_ESTIMATE) initial_t = 0.0;

    // Only look for leaf collisions.
    float node_collision_size_factor = 0.0;

    OctreeRaycastResult result = octree_raycast(
        ndc_xy,
        initial_t,
        node_collision_size_factor
    );

    if (result.hit) {
        color = result.color;
        color.a *= alpha;

        // Compute lighting using normal vector.
        color.rgb *= compute_lighting(result.normal);

        // Compute fog using the position where the ray intersects the node.
        color = foggify_color(result.pos, color);

        // Compute depth buffer value.
        vec4 clip_pos = matrix * vec4(result.pos, 1.0);
        float ndc_depth = clip_pos.z / clip_pos.w;
        gl_FragDepth = 0.5 * (
            gl_DepthRange.diff * ndc_depth
                + gl_DepthRange.near
                + gl_DepthRange.far
        );
    } else {
        // The ray did not intersect any cell.
        if (!OVERLAY_ITERATIONS && !OVERLAY_DEPTH_ESTIMATE) discard;
    }


    // Debug overlays
    if (OVERLAY_ITERATIONS) {
        float iters = float(result.iterations);
        if (iters <= 4.0) discard;
        color.rgb = mix(color.rgb, vec3(0.1 + iters / 150.0), 0.5);
        color.a = 1.0;
    }
    if (OVERLAY_DEPTH_ESTIMATE) {
        color.rg = mix(color.rg, 1 - vec2(t_lower_bound) / vec2(10.0, 100.0), 0.5);
        color.a = max(color.a, 0.5);
    }
}
