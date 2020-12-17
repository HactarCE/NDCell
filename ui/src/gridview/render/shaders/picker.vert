#version 140

in vec3 pos;
in uint target_id;

flat out uint vId;

uniform mat4 matrix;

void main() {
    gl_Position = matrix * vec4(pos, 1.0);
    vId = target_id;
}
