// This file is included in shader programs using an awful hack.
// See `shaders/mod.rs`.

uniform vec4 fog_color;
uniform vec3 fog_center; // center of fog sphere
uniform float fog_start; // radius at which fog starts
uniform float fog_end;   // radius at which fog reaches maximum

vec4 foggify_color(vec3 pos, vec4 unfogged_color) {
    float dist = distance(pos, fog_center);
    float visibility = 1 - smoothstep(fog_start, fog_end, dist);
    return mix(fog_color, unfogged_color, visibility);
}
