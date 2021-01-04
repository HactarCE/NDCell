// This file is included in shader programs using an awful hack.
// See `shaders/mod.rs`.

uniform vec4 fog_color;
uniform vec3 fog_center; // center of fog sphere
uniform float fog_start; // radius at which fog starts
uniform float fog_end;   // radius at which fog reaches maximum

vec4 foggify_color(vec3 pos, vec4 unfogged_color) {
    // Visibility is a piecewise linear function of distance.
    float dist = distance(pos, fog_center);
    float visibility = clamp((fog_end - dist) / (fog_end - fog_start), 0, 1);
    return mix(fog_color, unfogged_color, visibility);
}
