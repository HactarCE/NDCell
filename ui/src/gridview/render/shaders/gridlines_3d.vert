#version 150

in vec3 pos;
in vec3 normal;
in vec4 color; // ignored

out vec3 pos_3d; // XYZ coordinates in local space
out vec2 grid_pos; // XY coordinates on 2D grid

uniform mat4 matrix;

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

const float EPSILON = 1.0 / 256.0;

void main() {
    gl_Position = matrix * vec4(pos, 1.0);
    gl_Position.z -= EPSILON; // Gridlines appear in front of cells.

    // Identify the axes that are parallel to the plane.
    int ax1; if (normal.x == 0.0) { ax1 = 0; } else { ax1 = 1; }
    int ax2; if (normal.z == 0.0) { ax2 = 2; } else { ax2 = 1; }

    pos_3d = pos;

    vec2 pos_2d = vec2(pos[ax1], pos[ax2]);
    vec2 origin_2d = vec2(grid_origin[ax1], grid_origin[ax2]);
    grid_pos = (pos_2d - origin_2d) / grid_coefficient;
}
