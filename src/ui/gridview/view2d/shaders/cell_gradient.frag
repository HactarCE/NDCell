#version 140

flat in uint cell_state;

// Colors on each end of the gradient
uniform vec4 color1;
uniform vec4 color2;
// Cell states on each end of the gradient (state1 must be strictly less than state2)
uniform uint state1;
uniform uint state2;

// Color to use for cells that are not in the gradient
uniform vec4 default_color;

out vec4 color;

void main() {
    if (cell_state < state1) {
        // The state is outside the gradient
        color = default_color;
    } else if (state2 < cell_state) {
        // The state is outside the gradient
        color = default_color;
    } else {
        // The state is within the gradient
        float gradient_len = float(color2 - color1);
        float gradient_pos = float(state2 - cell_state);

        float color1_multiplier = gradient_pos / gradient_len;
        float color2_multiplier = 1 - color1_multiplier;

        // Undo gamma correction
        vec4 rgb_color1 = pow(color1, vec4(2.2));
        vec4 rgb_color2 = pow(color2, vec4(2.2));

        vec4 rgb_result = rgb_color1 * color1_multiplier + rgb_color2 * color2_multiplier;

        // Let the graphics driver gamma correct in the other direction
        color = rgb_result;

        // This shader outputs RGB, not sRGB
    }
}
