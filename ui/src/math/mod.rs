pub mod bresenham;
mod face;
pub mod raycast;

pub use face::Face;

pub fn f32_color_to_u8([r, g, b, a]: [f32; 4]) -> [u8; 4] {
    [
        (r * 255.0) as u8,
        (g * 255.0) as u8,
        (b * 255.0) as u8,
        (a * 255.0) as u8,
    ]
}
