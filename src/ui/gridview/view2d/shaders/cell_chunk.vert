#version 140

// Cell state
in uint state;

// The position of this vertex within the cell
in vec2 pos;

// Outputs for the fragment shader to determine the color of this vertex
flat out uint cell_state;

// Coordinates of the chunk (in chunk-space)
uniform vec2 chunk_pos;

// Length of a chunk along one axis
uniform float chunk_size;

// Transformation matrix for the whole view
uniform mat4 matrix;

void main() {
    // Compute X and Y within chunk based on index within the chunk
    float x = mod(gl_InstanceID, chunk_size);
    float y = floor(gl_InstanceID / chunk_size);

    // Compute cell position relative to the visible space
    vec2 cell_pos = chunk_pos * chunk_size + vec2(x, y) + pos;

    // Multiply that by the transformation matrix
    gl_Position = matrix * vec4(cell_pos, 0.0, 1.0);

    // Pass the cell state to the fragment shader
    cell_state = state;
}
