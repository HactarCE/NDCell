// This file is included in shader programs using an awful hack.
// See `shaders/mod.rs`.

layout(std140) uniform FogParams {
    vec3 fog_color;
    float fog_start; // radius at which fog starts
    vec3 fog_center; // center of fog sphere
    float fog_end;   // radius at which fog reaches maximum
};

vec4 foggify_color(vec3 pos, vec4 unfogged_color) {
    float dist = distance(pos, fog_center);
    float visibility = 1 - smoothstep(fog_start, fog_end, dist);
    vec3 rgb = mix(fog_color.rgb, unfogged_color.rgb, visibility);
    return vec4(rgb, unfogged_color.a);
}
