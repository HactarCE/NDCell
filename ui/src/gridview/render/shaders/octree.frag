// Octree traversal algorithm based on An Efficient Parametric Algorithm for
// Octree Traversal by J. Revelles, C. Ureña, M. Lastra.

#version 140

in vec2 screen_pos; // -1.0 ... +1.0 (same as `gl_Position`)

out vec4 color;

uniform mat4 matrix;
mat4 inv_matrix = inverse(matrix);

// One "render cell" per node; each pixel contains four uints (2 pixels per node).
uniform usampler2D octree_texture;
uniform int layer_count;
uniform uint root_idx;

uniform ivec3 octree_offset;
uint texture_width = uint(textureSize(octree_texture, 0).x);
float octree_side_len = (1 << layer_count);

uniform bool perf_view;

// These lines include other files in this GLSL program. See `shaders/mod.rs`.
//#include util/fog.frag
//#include util/lighting.frag

// Returns the value of the given child of the given node. `node` is the index
// of the node; `which_child` describes the position of the child along the X,
// Y, and Z axes using bits 0, 1, and 2 respectively.
uint getNodeChild(uint node, bvec3 which_child) {
    uint texel_index = (node << 1) + uint(which_child.z); // node*2 + Z
    ivec2 texel_pos = ivec2(
        texel_index % texture_width,
        texel_index / texture_width
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
// `tm` at which the ray crosses the middle of a node along each axis, returns
// the `bvec3` of the first child of that node intersected by the ray.
bvec3 entryChild(vec3 t0, vec3 tm) {
    float max_t0 = vec3_max(t0); // when the ray actually enters the node
    return greaterThanEqual(vec3(max_t0), tm);
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

void main() {
    color = vec4(0.0); // Assume transparent.
    gl_FragDepth = 1.0; // Assume far plane.

    // Compute the ray for this pixel. (based on
    // https://stackoverflow.com/a/42634961)
    vec3 start, delta;
    {
        vec4 near = inv_matrix * vec4(screen_pos, 0.0, 1.0);
        vec4 far = near + inv_matrix[2];
        near.xyz /= near.w;
        far.xyz /= far.w;

        start = near.xyz;                      // ray start position
        delta = normalize(far.xyz - near.xyz); // ray delta vector
    }

    vec3 original_start = start;
    vec3 original_delta = delta;

    // Check each component of the delta vector to see if it's negative. If it
    // is, then mirror the ray along that axis so that the delta vector is
    // positive and also mirror the quadtree along that axis using
    // `invert_mask`, which will flip bits of child indices.
    bvec3 invert_mask = lessThan(delta, vec3(0));
    start -= octree_offset;
    start = mix(start, octree_side_len - start, invert_mask);
    delta = mix(delta, -delta, invert_mask);

    // At what `t` does the ray enter the root node (considering only one axis
    // at a time)?
    vec3 t0 = -start / delta;
    // At what `t` does the ray exit the root node (considering only one axis at
    // a time)?
    vec3 t1 = (octree_side_len - start) / delta;
    // At what `t` does the ray cross the middle of the root node (considering
    // only one axis at a time)?
    vec3 tm = (t0 + t1) / 2.0;

    // At what `t` has the ray entered the root node along all axes?
    float max_t0 = vec3_max(t0);
    // At what `t` has the ray entered the root node along all axes?
    float min_t1 = vec3_min(t1);
    // If we enter AFTER we exit, or exit at a negative `t` ...
    if (max_t0 >= min_t1 || 0 > min_t1) {
        // ... then the ray does not intersect with the root node.
        discard;
    } else {
        // Otherwise, the ray does intersect with the root node.

        // GLSL doesn't support recursion, so we keep a stack for each local
        // variable. The octree should never be more than 32 layers deep because
        // that would cause precision problems and stuff, so that should be
        // plenty of stack space.
        int layer = layer_count;    // current layer (used as index into stack)
        uint [32] node_idx_stack;   // index of current node
        bool [32] has_next_child;   // whether the ray intersects another child of the current node
        bvec3[32] next_child_stack; // index of the child of the current node that the ray intersects next
        vec3 [32] t0_stack;         // time of entry along axis
        vec3 [32] tm_stack;         // time of reaching middle along axis
        vec3 [32] t1_stack;         // time of exit along axis

        // Set initial stack values.
        node_idx_stack[layer] = root_idx;
        next_child_stack[layer] = entryChild(t0, tm);
        has_next_child[layer] = true;
        t0_stack[layer] = t0;
        tm_stack[layer] = (t0 + t1) / 2.0;
        t1_stack[layer] = t1;

        int count = 0;

        while (layer <= layer_count) {
            if (!has_next_child[layer]) {
                // The ray has exited the current node, so backtrack up a layer.
                layer++;
            } else {
                count++;

                bvec3 next_child = next_child_stack[layer];

                // Compute the parameter `t` values for the `next_child`.
                t0 = mix(t0_stack[layer], tm_stack[layer], next_child);
                t1 = mix(tm_stack[layer], t1_stack[layer], next_child);
                tm = (t0 + t1) / 2.0;

                // Compute the next sibling of `next_child` to visit after this
                // one.
                uint exit_axis = exitAxis(t1);
                // `== true` is required to work around a bug in AMD drivers.
                // Yes this is cursed.
                if (next_child[exit_axis] == true) {
                    // `next_child` is the last child of the current node that
                    // the ray intersects.
                    has_next_child[layer] = false;
                } else {
                    // Advance along `exit_axis` to get the next child to visit.
                    next_child_stack[layer][exit_axis] = true;
                }

                if (vec3_min(t1) < 0) {
                    // This node is completely behind the camera; skip it.
                    continue;
                } else if (vec3_max(t0) < 0 && layer == 1) {
                    // This is a leaf node and the camera is inside it; skip it.
                    continue;
                }

                bvec3 tmp_child_index = notEqual(next_child, invert_mask); // logical XOR
                uint child_value = getNodeChild(node_idx_stack[layer], tmp_child_index);
                if (child_value == 0u) {
                    // This is an empty node; skip it.
                } else if (layer > 1) {
                    // This is a non-leaf node; set stack values and descend one
                    // layer.
                    layer--;
                    node_idx_stack[layer] = child_value;
                    has_next_child[layer] = true;
                    next_child_stack[layer] = entryChild(t0, tm);
                    t0_stack[layer] = t0;
                    tm_stack[layer] = tm;
                    t1_stack[layer] = t1;
                } else {
                    // This is a nonzero leaf node, so set the color and break
                    // out of the loop.

                    // RGBA, big-endian; convert from 0-255 to 0.0-1.0
                    float r = float((child_value >> 24) & 255u) / 255.0;
                    float g = float((child_value >> 16) & 255u) / 255.0;
                    float b = float((child_value >>  8) & 255u) / 255.0;
                    float a = float( child_value        & 255u) / 255.0;
                    color = vec4(r, g, b, a);

                    // Compute lighting using a normal vector based on the entry
                    // axis for the node.
                    vec3 normal = vec3(0.0);
                    uint axis = entryAxis(t0);
                    normal[axis] = mix(-1, 1, invert_mask[axis]);
                    color.rgb *= compute_lighting(normal);

                    // Compute fog using the position where the ray intersects
                    // the node.
                    vec3 pos = original_start + original_delta * vec3_max(t0);
                    color = foggify_color(pos, color);

                    // Compute depth buffer value.
                    vec4 clip_pos = matrix * vec4(pos, 1.0);
                    float ndc_depth = clip_pos.z / clip_pos.w;
                    gl_FragDepth = 0.5 * (
                        gl_DepthRange.diff * ndc_depth
                            + gl_DepthRange.near
                            + gl_DepthRange.far
                    );

                    break; // Break out of the loop.
                }
            }
        }

        if (perf_view) {
            color /= 2.0;
            if (count < 50.0)
                color.r += 0.25 + count / 100.0;
            else if (count < 100.0)
                color.g += 0.25 + (count - 50.0) / 100.0;
            else if (count < 150.0)
                color.b += 0.25 + (count - 100.0) / 100.0;
            else
                color.rgb += vec3(0.5);
            color.a = max(color.a, 0.5);
        }

        // The ray did not intersect any cell.
        if (layer > layer_count) {
            if (perf_view)
                color.rgb /= 2.0;
            else
                discard;
        }
    }
}
