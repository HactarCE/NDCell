#version 140

/*
 * Pixel Mixing filter, also called Area Averaging Scale Filter and many other
 * names.
 *
 * (see https://stackoverflow.com/q/12526449
 *  and http://entropymine.com/imageworsener/pixelmixing/
 *  and https://www.desmos.com/calculator/sogqxjzxiw)
 *
 * Please forgive this explanation for talking in terms out pixels instead of
 * sample points. Pixels are more relevant to the desired input/output, even
 * though the algorithm must be implemented using sampling.
 *
 * This algorithm essentially overlays a new grid on top of the old one, and
 * then averages the color within each square -- which is exactly what we want
 * for converting a square CA grid into a square pixelated image.
 *
 * We use bilinear scaling internally, so scaling down by more than a factor of
 * 2 ignores some pixels.
 */

in vec2 tex_coords;

out vec4 color;

uniform float alpha;

uniform sampler2D src_texture;

// Scale factor
uniform float scale_factor;
// Slope of interpolation; if scaling down, use p=1 (equivalent to linear
// interpolation).
float p = max(scale_factor, 1.0);

// Pixel size of the source texture
vec2 tex_size = vec2(textureSize(src_texture, 0));
// Pixel size of the part of the source texture being used
uniform vec2 active_tex_size;

void main() {
    // Determine the (floating-point) pixel position in the source texture.
    vec2 uv = tex_coords * active_tex_size;

    // Round to the nearest pixel boundary. (Pixels are sampled at
    // half-integers, not integers).
    vec2 uv_i = round(uv);
    // Compute the signed distance from that pixel boundary.
    vec2 uv_f = uv - uv_i;
    // See https://www.desmos.com/calculator/sogqxjzxiw.
    uv = uv_i + clamp(uv_f * p, -0.5, 0.5);

    // Convert back from pixels to the range [0, 1].
    vec2 new_tex_coords = uv / tex_size;

    // Sample the point using linear interpolation.
    color = texture(src_texture, new_tex_coords).rgba;

    // Multiply alpha by the uniform value.
    color.a *= alpha;
}
