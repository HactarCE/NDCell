// Octree traversal algorithm based on An Efficient Parametric Algorithm for
// Octree Traversal by J. Revelles, C. Ureña, M. Lastra.

#version 150

const bool SHOW_ITERATIONS = false;

in vec2 ndc_xy; // -1.0 ... +1.0 (same as `gl_Position`)

out vec4 color;

uniform float alpha;

// These lines include other files in this GLSL program. See `shaders/mod.rs`.
//#include util/fog.frag
//#include util/lighting.frag
//#include util/octree.frag

void main() {
    color = vec4(0.0);  // Assume transparent.
    gl_FragDepth = 1.0; // Assume far plane.

    float initial_t = 0.0;
    OctreeRaycastResult result = octree_raycast(initial_t);

    if (result.hit) {
        color = result.color;

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
        if (!SHOW_ITERATIONS) discard;
    }


    // Performance debug view
    if (SHOW_ITERATIONS) {
        uint iters = result.iterations;
        color /= 2.0;
        if (iters < 50.0)       color.r += 0.25 + iters / 100.0;
        else if (iters < 100.0) color.g += 0.25 + (iters - 50.0) / 100.0;
        else if (iters < 150.0) color.b += 0.25 + (iters - 100.0) / 100.0;
        else                    color.rgb += vec3(0.5);
        color.a = max(color.a, 0.5);
        if (!result.hit) color.rgb /= 2.0;
    }

}
