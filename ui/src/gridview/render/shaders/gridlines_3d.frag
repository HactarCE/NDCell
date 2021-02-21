#version 150

in vec3 pos_3d;
in vec2 grid_pos;

out vec4 color;

layout(std140) uniform GridlineParams {
    vec4 grid_color;

    vec3 grid_origin;

    float grid_width; // measured as a fraction of a render cell

    ivec3 grid_max_exponents;
    float grid_coefficient;
    int grid_base; // exponential base

    float grid_max_spacing;
    float grid_min_spacing;
};

// This line includes another file in this GLSL program. See `shaders/mod.rs`.
//#include util/fog.frag

// Find the largest possible exponent for this particular gridline, which will
// determine its opacity. I couldn't find an explicit formula for "largest power
// of N that divides M" so we just have to use a loop.
ivec2 gridline_exponent(ivec2 nearest_line) {
    // Amortized over all gridlines, this algorithm takes O(1) time per pixel
    // because the sum of the reciprocals of powers of N>=2 converges; each
    // `i`th term in the infinite sum represents the proportion of gridlines
    // that reach the `i`th iteration of the loop.
    ivec2 exponent = ivec2(0);
    int tmp;

    tmp = abs(nearest_line.x);
    if (grid_base >= 2 && tmp != 0) {
        while (tmp % grid_base == 0 && exponent.x < grid_max_exponents.x) {
            tmp /= grid_base;
            exponent.x++;
        }
    } else {
        // In this case we already know that the `tmp % grid_base == 0`
        // condition will always be true.
        exponent.x = grid_max_exponents.x;
    }

    tmp = abs(nearest_line.y);
    if (grid_base >= 2 && tmp != 0) {
        while (tmp % grid_base == 0 && exponent.y < grid_max_exponents.y) {
            tmp /= grid_base;
            exponent.y++;
        }
    } else {
        // In this case we already know that the `tmp % grid_base == 0`
        // condition will always be true.
        exponent.y = grid_max_exponents.y;
    }

    return exponent;
}

void main() {
    float w = 1.0 / gl_FragCoord.w;
    float z = gl_FragCoord.z / gl_FragCoord.w;

    // The gridline must be at least one pixel thick.
    vec2 delta = fwidth(grid_pos); // d(grid pos)/d(pixel pos)
    vec2 line_width = max(vec2(grid_width), delta);

    // Convert `grid_(min|max)_spacing` from pixels to cells, and then take the
    // `grid_base`th root (which we can do using division in logarithm land).
    float log2_grid_base = log2(float(grid_base));
    vec2 min_exponent = log2(grid_min_spacing * delta) / log2_grid_base;
    vec2 max_exponent = log2(grid_max_spacing * delta) / log2_grid_base;

    // Compute the exponent of the smallest possible gridline that would be
    // visible. The exponent must be positive, because we never show gridlines
    // with negative exponent.
    vec2 int_min_exponent = max(vec2(0), ceil(min_exponent));
    // Compute the number of render cells between each of those smallest
    // possible gridlines.
    vec2 min_cell_spacing = pow(vec2(grid_base), int_min_exponent);
    // Round the position to the nearest of those smallest possible gridlines.
    vec2 int_nearest_line = round(grid_pos / min_cell_spacing);
    vec2 nearest_line = int_nearest_line * min_cell_spacing;
    // Compute the distance of this fragment from that nearest gridline.
    vec2 dist_from_line = abs(nearest_line - grid_pos);
    // And use that distance along with the desired line width to determine the
    // opacity of the this particular fragment. This `smoothstep()` call defines
    // a range equivalent to the size of one fragment, centered on this current
    // fragment, and then measures where `line_width` lies within that range.
    vec2 alpha = smoothstep(
        dist_from_line - delta / 2.0, // half a pixel left/down
        dist_from_line + delta / 2.0, // half a pixel up/right
        line_width             / 2.0  // only consider one half of the line
    );

    // Now find the largest possible exponent for this particular gridline,
    // which will help determine its opacity at this point. This function is
    // defined above.
    vec2 exponent = int_min_exponent + vec2(gridline_exponent(ivec2(int_nearest_line)));
    alpha *= smoothstep(min_exponent, max_exponent, exponent);

    color = grid_color;
    color.a *= max(alpha.x, alpha.y);
    color = foggify_color(pos_3d, color);
}
