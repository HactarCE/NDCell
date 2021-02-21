use super::*;

#[derive(Debug, Copy, Clone)]
pub(super) struct LightingParams {
    light_direction: [f32; 3],
    light_ambientness: f32,
    light_multiplier: f32,
}

implement_uniform_block!(
    LightingParams,
    light_direction,
    light_ambientness,
    light_multiplier,
);

impl Default for LightingParams {
    fn default() -> Self {
        Self {
            light_direction: LIGHT_DIRECTION,
            light_ambientness: LIGHT_AMBIENTNESS,
            light_multiplier: LIGHT_MULTIPLIER,
        }
    }
}
