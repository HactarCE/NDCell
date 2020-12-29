#version 140

in vec3 pos;
in vec3 normal;
in vec4 color;

out vec3 vPos;
out vec4 vColor;

uniform mat4 matrix;

uniform vec3 light_direction;    // normalized vector
uniform float light_ambientness; // 0.0 ... 1.0
uniform float max_light;         // 0.0 ... 1.0

float light_scalar_factor = light_ambientness;
float light_vector_factor = (1.0 - light_ambientness);

void main() {
    gl_Position = matrix * vec4(pos, 1.0);
    vPos = pos;
    vColor = color;
    float light_vector_alignment = dot(normal, normalize(light_direction)) / 2.0 + 0.5;
    float color_multiplier = max_light * (light_scalar_factor + light_vector_factor * light_vector_alignment);
    vColor.rgb *= color_multiplier;
}
