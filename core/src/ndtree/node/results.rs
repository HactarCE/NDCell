use std::convert::TryInto;

use super::Layer;
use crate::num::{BigInt, BigUint, One, ToPrimitive, Zero};

/// Parameters that determine the meaning of the "result" of a node.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct HashLifeResultParams {
    /// Base-2 log of the rule radius.
    log2_rule_radius: u32,
    /// Base-2 log of the step size for the simulation.
    log2_step_size: u32,
    /// Number of power-of-2-sized steps to simulate each frame.
    num_steps: u32,
    /// Layer of the smallest node that can be simulated for the full step size.
    sim_base_layer: Layer,
    /// Layer of the smallest node that can be simulated for a single
    /// generation, which depends solely on the rule radius.
    min_layer: Layer,
}
impl Default for HashLifeResultParams {
    fn default() -> Self {
        Self::new().build()
    }
}
impl HashLifeResultParams {
    /// Creates a `HashLifeResultParamsBuilder`, which is used to build a
    /// `HashLifeResultParams`.
    pub fn new() -> HashLifeResultParamsBuilder {
        HashLifeResultParamsBuilder {
            log2_rule_radius: 0,
            log2_step_size: 0,
            num_steps: 1,
        }
    }

    /// Returns the base-2 log of the rule radius (rounded up).
    #[inline]
    pub fn log2_rule_radius(&self) -> u32 {
        self.log2_rule_radius
    }
    /// Returns the base-2 log of the simulation step size.
    #[inline]
    pub fn log2_step_size(&self) -> u32 {
        self.log2_step_size
    }
    /// Returns the number of power-of-2-sized steps per frame.
    #[inline]
    pub fn num_steps(&self) -> u32 {
        self.num_steps
    }
    /// Returns the layer of the smallest node that can be simulated for the
    /// full step size.
    #[inline]
    pub fn sim_base_layer(&self) -> Layer {
        self.sim_base_layer
    }
    /// Returns the layer of the smallest node that can be simulated for a
    /// single generation, which depends solely on the rule radius.
    #[inline]
    pub fn min_layer(&self) -> Layer {
        self.min_layer
    }

    /// Returns the simulation step size.
    #[inline]
    pub fn big_step_size(&self) -> BigUint {
        BigUint::one() << self.log2_step_size
    }
    /// Returns the step size for a node at the given layer.
    #[inline]
    pub fn big_node_step_size(&self, layer: Layer) -> BigUint {
        if let Some(pow) = self.log2_node_step_size(layer) {
            BigUint::one() << pow
        } else {
            BigUint::zero()
        }
    }
    /// Returns the step size for a node at the given layer, or `None` if it
    /// does not fit in a `usize`.
    #[inline]
    pub fn node_step_size(&self, layer: Layer) -> Option<usize> {
        if let Some(pow) = self.log2_node_step_size(layer) {
            1_usize.checked_shl(pow)
        } else {
            Some(0)
        }
    }
    /// Returns the base-2 log of the step size for a node at the given layer,
    /// or `None` if even a single generation cannot be simulated.
    #[inline]
    pub fn log2_node_step_size(&self, layer: Layer) -> Option<u32> {
        let sim_base_layer = self.sim_base_layer();
        if sim_base_layer < layer {
            return Some(self.log2_step_size);
        } else {
            let log2_divisor = sim_base_layer.to_u32() - layer.to_u32();
            // If the result of this subtraction is negative, then we can't even
            // simulate a single generation on this node.
            self.log2_step_size.checked_sub(log2_divisor)
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct HashLifeResultParamsBuilder {
    log2_rule_radius: u32,
    log2_step_size: u32,
    num_steps: u32,
}
impl HashLifeResultParamsBuilder {
    pub fn with_rule_radius(mut self, rule_radius: usize) -> Self {
        self.log2_rule_radius = rule_radius
            .next_power_of_two()
            .max(1) // Treat r=0 rules like r=1.
            .trailing_zeros();
        self
    }

    pub fn with_step_size(mut self, generations: &BigInt) -> Self {
        // TODO: return Result from this method so that bad step sizes don't
        // panic.

        // The number of trailing zeros (i.e. index of the first `1` bit) gives
        // the greatest power-of-2 multiple.
        let log2_step_size = generations
            .trailing_zeros()
            .unwrap()
            .try_into()
            .expect("Step size too large!");

        // If the number of generations is not a power of 2, we may have to
        // break this into multiple power-of-2-sized steps.
        let num_steps: BigInt = generations >> log2_step_size;
        let num_steps = num_steps.to_u32().expect(
            "Simulation requires too many individual steps; try using a power of 2 step size",
        );

        self.log2_step_size = log2_step_size;
        self.num_steps = num_steps;
        self
    }

    pub fn build(self) -> HashLifeResultParams {
        let log2_rule_radius = self.log2_rule_radius;
        let log2_step_size = self.log2_step_size;
        let num_steps = self.num_steps;

        // Distance = velocity * time. `rule_radius` is the velocity (cells per
        // generation) and the step size is the time (number of generations).
        let log2_distance = log2_rule_radius + log2_step_size;
        // Multiply by 4 because that is only the "padding" between the edge of
        // them node and the edge of its centered inner result.
        let log2_node_len = log2_distance + 2;
        let sim_base_layer = Layer(log2_node_len);

        let min_layer = Layer(log2_rule_radius + 2);

        HashLifeResultParams {
            log2_rule_radius,
            log2_step_size,
            num_steps,
            sim_base_layer,
            min_layer,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hashlife_node_step_size() {
        let _1x1 = Layer(0);
        let _2x2 = Layer(1);
        let _4x4 = Layer(2);
        let _8x8 = Layer(3);
        let _16x16 = Layer(4);
        let _32x32 = Layer(5);
        let _64x64 = Layer(6);

        for max_log2_step_size in 0..=5 {
            println!("Testing max_log2_step_size={}", max_log2_step_size);

            let m = 1 << max_log2_step_size;
            let b = HashLifeResultParams::new().with_step_size(&m.into());

            for r in 0..=1 {
                let p = b.with_rule_radius(r).build();
                assert_eq!(Some(m.min(0)), p.node_step_size(_1x1));
                assert_eq!(Some(m.min(0)), p.node_step_size(_2x2));
                assert_eq!(Some(m.min(1)), p.node_step_size(_4x4));
                assert_eq!(Some(m.min(2)), p.node_step_size(_8x8));
                assert_eq!(Some(m.min(4)), p.node_step_size(_16x16));
                assert_eq!(Some(m.min(8)), p.node_step_size(_32x32));
                assert_eq!(Some(m.min(16)), p.node_step_size(_64x64));
            }

            let p = b.with_rule_radius(2).build();
            assert_eq!(Some(m.min(0)), p.node_step_size(_1x1));
            assert_eq!(Some(m.min(0)), p.node_step_size(_2x2));
            assert_eq!(Some(m.min(0)), p.node_step_size(_4x4));
            assert_eq!(Some(m.min(1)), p.node_step_size(_8x8));
            assert_eq!(Some(m.min(2)), p.node_step_size(_16x16));
            assert_eq!(Some(m.min(4)), p.node_step_size(_32x32));
            assert_eq!(Some(m.min(8)), p.node_step_size(_64x64));

            for r in 3..=4 {
                let p = b.with_rule_radius(r).build();
                assert_eq!(Some(m.min(0)), p.node_step_size(_1x1));
                assert_eq!(Some(m.min(0)), p.node_step_size(_2x2));
                assert_eq!(Some(m.min(0)), p.node_step_size(_4x4));
                assert_eq!(Some(m.min(0)), p.node_step_size(_8x8));
                assert_eq!(Some(m.min(1)), p.node_step_size(_16x16));
                assert_eq!(Some(m.min(2)), p.node_step_size(_32x32));
                assert_eq!(Some(m.min(4)), p.node_step_size(_64x64));
            }

            let p = b.with_rule_radius(5).build();
            assert_eq!(Some(m.min(0)), p.node_step_size(_1x1));
            assert_eq!(Some(m.min(0)), p.node_step_size(_2x2));
            assert_eq!(Some(m.min(0)), p.node_step_size(_4x4));
            assert_eq!(Some(m.min(0)), p.node_step_size(_8x8));
            assert_eq!(Some(m.min(0)), p.node_step_size(_16x16));
            assert_eq!(Some(m.min(1)), p.node_step_size(_32x32));
            assert_eq!(Some(m.min(2)), p.node_step_size(_64x64));
        }
    }
}
