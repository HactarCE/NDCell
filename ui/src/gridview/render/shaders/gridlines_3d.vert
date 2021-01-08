#version 140

in vec2 pos;

out vec3 pos_3d;
out vec2 grid_pos; // XY coordinates on 2D grid

uniform mat4 matrix;

uniform ivec2 grid_axes;
uniform vec3 grid_origin; // origin for gridlines
uniform float grid_coefficient;

const float epsilon = 1.0 / 256.0;

void main() {
    int ax1 = grid_axes.x;
    int ax2 = grid_axes.y;

    pos_3d = grid_origin;
    pos_3d[ax1] += pos.x;
    pos_3d[ax2] += pos.y;

    gl_Position = matrix * vec4(pos_3d, 1.0);
    gl_Position.z -= epsilon; // Gridlines appear in front of cells.

    grid_pos = pos / grid_coefficient;
}
