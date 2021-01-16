#version 150

in vec2 pos;

out vec3 pos_3d;
out vec2 grid_pos; // XY coordinates on 2D grid

uniform mat4 matrix;

uniform ivec2 grid_axes;
uniform float other_coordinate;

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

const float epsilon = 1.0 / 256.0;

void main() {
    int ax1 = grid_axes.x;
    int ax2 = grid_axes.y;

    pos_3d = vec3(other_coordinate);
    pos_3d[ax1] = pos.x;
    pos_3d[ax2] = pos.y;

    gl_Position = matrix * vec4(pos_3d, 1.0);
    gl_Position.z -= epsilon; // Gridlines appear in front of cells.

    grid_pos = (pos - vec2(grid_origin[ax1], grid_origin[ax2])) / grid_coefficient;
}
