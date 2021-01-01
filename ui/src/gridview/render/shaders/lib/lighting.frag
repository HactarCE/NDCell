// This file is included in shader programs using an awful hack.
// See `shaders/mod.rs`.

uniform vec3 light_direction;    // normalized vector
uniform float light_ambientness; // 0.0 ... 1.0
uniform float max_light;         // 0.0 ... 1.0

float light_scalar_factor = light_ambientness;
float light_vector_factor = 1.0 - light_ambientness;

float compute_lighting(vec3 normal) {
    float light_vector_alignment = dot(normal, normalize(light_direction)) / 2.0 + 0.5;
    float color_multiplier = max_light * (light_scalar_factor + light_vector_factor * light_vector_alignment);
    return color_multiplier;
}
