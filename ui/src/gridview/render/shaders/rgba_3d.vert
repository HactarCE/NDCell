#version 150

in vec3 pos;
in vec3 normal;
in vec4 color;

out vec3 vPos;
out vec4 vColor;

uniform mat4 matrix;

const float EPSILON = 1.0 / 256.0;

// This line includes another file in this GLSL program. See `shaders/mod.rs`.
//#include util/lighting.frag

void main() {
    gl_Position = matrix * vec4(pos, 1.0);
    if (color.a < 1.0) {
        // Avoid Z-fighting when something transparent is drawn in the same
        // position as something opaque; let the transparent thing be in front.
        gl_Position.z -= EPSILON;
    }

    vPos = pos;
    vColor = color;
    vColor.rgb *= compute_lighting(normal);
}
