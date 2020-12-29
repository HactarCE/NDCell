#version 140

in vec3 vPos;
in vec4 vColor;

out vec4 color;

uniform vec4 fog_color;
uniform vec3 fog_center; // center of fog sphere
uniform float fog_start; // radius at which fog starts
uniform float fog_end;   // radius at which fog reaches maximum

void main() {
    // Visibility is a piecewise linear function of distance.
    float dist = distance(vPos, fog_center);
    float visibility = clamp((fog_end - dist) / (fog_end - fog_start), 0, 1);

    color = mix(fog_color, vColor, visibility);
}
