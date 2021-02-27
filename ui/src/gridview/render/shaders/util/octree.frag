// Octree traversal algorithm based on An Efficient Parametric Algorithm for
// Octree Traversal by J. Revelles, C. Ureña, M. Lastra.
// (http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.29.987)

// This file is included in shader programs using an awful hack.
// See `shaders/mod.rs`.

layout(std140) uniform OctreeParams {
    mat4 matrix;
    mat4 inv_matrix;

    ivec3 octree_base;
    int layer_count;
    uint root_idx;
};

// One "render cell" per node; each pixel contains four uints (2 pixels per node).
uniform usampler2D octree_texture;

uint octree_texture_width = uint(textureSize(octree_texture, 0).x);
float octree_side_len = (1 << layer_count);

// Returns the value of the given child of the given node. `node` is the index
// of the node; `which_child` describes the position of the child along the X,
// Y, and Z axes using bits 0, 1, and 2 respectively.
uint getNodeChild(uint node, bvec3 which_child) {
    uint texel_index = (node << 1) + uint(which_child.z); // node*2 + Z
    ivec2 texel_pos = ivec2(
        texel_index % octree_texture_width,
        texel_index / octree_texture_width
    );

    uint child_index = uint(which_child.x)
                     | uint(which_child.y) << 1; // X + Y*2

    uvec4 children = texelFetch(octree_texture, texel_pos, 0);
    return children[child_index];
}

float vec3_max(vec3 v) {
    return max(v.x, max(v.y, v.z));
}
float vec3_min(vec3 v) {
    return min(v.x, min(v.y, v.z));
}

// Given the parameters `t0` at which the ray enters a node along each axis and
// `t1` at which the ray exits a node along each axis, returns the `bvec3` of
// the first child of that node intersected by the ray, converted to a `vec3`
// with values `0.0` or `0.5`.
vec3 entryChild(vec3 t0, vec3 t1) {
    // At what `t` does the ray cross the middle of the root node (considering
    // only one axis at a time)?
    vec3 tm = mix(t0, t1, 0.5);
    // At what `t` has the ray entered the node along all axes; i.e. it has
    // actually entered the node?
    float max_t0 = vec3_max(t0);
    // Return 0.5 for each axis where the ray crosses the halfway point before
    // entering the node along all axes.
    return vec3(greaterThanEqual(vec3(max_t0), tm)) * 0.5;
}

// Given the parameters `t0` at which the ray enters a node along each axis,
// returns an integer corresponding to the axis along which the ray enters the
// node: X=0, Y=1, and Z=2.
uint entryAxis(vec3 t0) {
    if (t0.x > t0.y) {
        if (t0.x > t0.z) return 0u; // X>Y && X>Z
        else             return 2u; // Z>X>Y
    } else {
        if (t0.y > t0.z) return 1u; // Y>X && Y>Z
        else             return 2u; // Z>Y>X
    }
}

// Given the parameters `t1` at which the ray exits a node along each axis,
// returns an integer corresponding to the axis along which the ray exits the
// node: X=0, Y=1, and Z=2.
uint exitAxis(vec3 t1) {
    if (t1.x < t1.y) {
        if (t1.x < t1.z) return 0u; // X<Y && X<Z
        else             return 2u; // Z<X<Y
    } else {
        if (t1.y < t1.z) return 1u; // Y<X && Y<Z
        else             return 2u; // Z<Y<X
    }
}

struct OctreeRaycastResult {
    bool hit;
    float t;
    uint iterations;

    // These members may be uninitialized if `hit` is false.
    bool leaf;
    vec3 pos;
    int layer;

    // These members may be uninitialized if `hit` or `leaf` is false.
    vec4 color;
    vec3 normal;
};

OctreeRaycastResult octree_raycast(vec2 ndc_xy, float t_start, float node_collision_size_factor) {
    // Compute the ray for this pixel. (based on
    // https://stackoverflow.com/a/42634961)
    vec3 start, delta;
    {
        vec4 near = inv_matrix * vec4(ndc_xy, 0.0, 1.0);
        vec4 far = near + inv_matrix[2];
        near.xyz /= near.w;
        far.xyz /= far.w;

        start = near.xyz;                      // ray start position
        delta = normalize(far.xyz - near.xyz); // ray delta vector
    }

    vec3 pixel_width_start = fwidth(start);
    vec3 pixel_width_delta = fwidth(delta);

    vec3 original_start = start;
    vec3 original_delta = delta;

    // Check each component of the delta vector to see if it's negative. If it
    // is, then mirror the ray along that axis so that the delta vector is
    // positive and also mirror the quadtree along that axis using
    // `invert_mask`, which will flip bits of child indices.
    vec3 invert_mask = vec3(lessThan(delta, vec3(0)));
    start -= octree_base;
    start = mix(start, octree_side_len - start, invert_mask);
    delta = mix(delta, -delta, invert_mask);

    // At what `t` does the ray enter the root node (considering only one axis
    // at a time)?
    vec3 t0 = -start / delta;
    // At what `t` does the ray exit the root node (considering only one axis at
    // a time)?
    vec3 t1 = (octree_side_len - start) / delta;

    // At what `t` has the ray entered the root node along all axes?
    float max_t0 = vec3_max(t0);
    // At what `t` has the ray entered the root node along all axes?
    float min_t1 = vec3_min(t1);
    // If we enter AFTER we exit, or exit at a negative `t` ...
    if (max_t0 >= min_t1 || 0 > min_t1) {
        // ... then the ray does not intersect with the root node.
        discard;
    }
    // Otherwise, the ray does intersect with the root node.

    // GLSL doesn't support recursion, so we keep a stack for each local
    // variable. The octree should never be more than 32 layers deep because
    // that would cause precision problems and stuff, so that should be
    // plenty of stack space.
    int layer = layer_count;   // current layer (used as index into stack)
    uint[32] node_idx_stack;   // index of current node
    vec3[32] t0_stack;         // time of entry along each axis
    vec3[32] t1_stack;         // time of exit along each axis
    vec3[32] next_child_stack; // position of the child of the current node that the ray
                               // intersects next (0 = low branch, 0.5 = high branch, 1.0 = exited)

    // Set initial stack values.
    node_idx_stack[layer] = root_idx;
    t0_stack[layer] = t0;
    t1_stack[layer] = t1;
    next_child_stack[layer] = entryChild(t0, t1);

    uint iterations = 0u;

    while (layer <= layer_count) {
        vec3 next_child = next_child_stack[layer];

        if (next_child.x == 1.0 || next_child.y == 1.0 || next_child.z == 1.0) {
            // The ray has exited the current node, so backtrack up a layer.
            layer++;
        } else {
            iterations++;

            // Compute the parameter `t` values for the `next_child`.
            t0 = mix(t0_stack[layer], t1_stack[layer], next_child);
            t1 = mix(t0_stack[layer], t1_stack[layer], next_child + 0.5);

            // Compute the sibling of `next_child` to visit after this one.
            uint exit_axis = exitAxis(t1);
            // Advance along `exit_axis` to get the next child to visit.
            next_child_stack[layer][exit_axis] += 0.5;

            if (
                // This node is completely behind the camera; skip it.
                t1.x < t_start || t1.y < t_start || t1.z < t_start
                // This is a leaf node and the camera is inside it; skip it.
                || layer == 1 && t0.x < 0 && t0.y < 0 && t0.z < 0
            ) continue;

            bvec3 tmp_child_index = notEqual(next_child * 2.0, invert_mask); // logical XOR
            uint child_value = getNodeChild(node_idx_stack[layer], tmp_child_index);
            if (child_value == 0u) {
                // This is an empty node; skip it.
            } else if (layer > 1 || node_collision_size_factor > 0.0) {
                // This is a non-leaf node; first, check if it's small enough
                // for a collision anyway.
                float node_width = float(1 << layer);
                float pixel_width = vec3_max(pixel_width_start + pixel_width_delta * vec3_min(t1));
                if (node_width < pixel_width * node_collision_size_factor || layer == 1) {
                    // The node is small enough that it's only a fraction of a
                    // pixel large; return a hit.
                    bool hit = true;
                    vec3 t_before = mix(t0, t1, vec3(-1.0)); // move back one node
                    float t = vec3_max(t_before);

                    bool leaf = false;
                    vec3 pos = original_start + original_delta * t;
                    vec4 color; vec3 normal; // uninitialized

                    color = vec4(pixel_width, 0.0, 0.0, 1.0);

                    return OctreeRaycastResult(
                        hit, t, iterations,
                        leaf, pos, layer,
                        color, normal
                    );
                }

                // It's not small enough, so set stack values and descend one
                // layer.
                layer--;
                node_idx_stack[layer] = child_value;
                t0_stack[layer] = t0;
                t1_stack[layer] = t1;
                next_child_stack[layer] = entryChild(t0, t1);
            } else {
                // This is a nonzero leaf node, so return from the function.
                bool hit = true;
                float t = vec3_max(t0);

                bool leaf = true;
                vec3 pos = original_start + original_delta * t;

                // Color is RGBA, big-endian; convert from 0-255 to 0.0-1.0
                float r = float((child_value >> 24) & 255u) / 255.0;
                float g = float((child_value >> 16) & 255u) / 255.0;
                float b = float((child_value >>  8) & 255u) / 255.0;
                float a = float( child_value        & 255u) / 255.0;
                vec4 color = vec4(r, g, b, a);

                // Compute normal vector based on the entry axis for the node.
                vec3 normal = vec3(0.0);
                uint axis = entryAxis(t0);
                normal[axis] = mix(-1.0, 1.0, invert_mask[axis]);

                return OctreeRaycastResult(
                    hit, t, iterations,
                    leaf, pos, layer,
                    color, normal
                );
            }
        }
    }

    bool hit = false;
    float t = min_t1;
    vec3 pos = original_start + original_delta * t;

    bool leaf; vec4 color; vec3 normal; // uninitialized

    return OctreeRaycastResult(
        hit, t, iterations,
        leaf, pos, layer,
        color, normal
    );
}
