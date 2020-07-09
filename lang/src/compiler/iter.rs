use itertools::Itertools;

use inkwell::types::VectorType;
use inkwell::values::{BasicValueEnum, IntValue, VectorValue};
use inkwell::IntPredicate;

use super::{const_int, const_uint, Compiler, PatternValue, Value};
use crate::LangResult;

impl Compiler {
    /// Compiles a (possibly unrolled) for loop using the given closure to generate code for each iteration.
    ///
    /// The iteration value is passed to the closure.
    pub fn build_value_iter(
        &mut self,
        value: Value,
        mut compile_for_each: impl FnMut(&mut Self, Value) -> LangResult<()>,
    ) -> LangResult<()> {
        match value {
            Value::Vector(v) => {
                self.build_vector_iter(v, |c, component| compile_for_each(c, Value::Int(component)))
            }
            Value::Pattern(p) => self.build_pattern_iter(&p, |c, _pos, cell_state_value| {
                compile_for_each(c, Value::CellState(cell_state_value))
            }),
            Value::IntRange(r) => {
                self.build_int_range_iter(r, |c, i| compile_for_each(c, Value::Int(i)))
            }
            Value::Rectangle(_) => {
                todo!("loop over positions in rectangle");
            }
            Value::CellStateFilter(_) => {
                todo!("loop over cell states matching filter");
            }
            _ => internal_error!("Don't know how to iterate over type {}", value.ty()),
        }
    }

    pub fn build_vector_iter(
        &mut self,
        vector: VectorValue<'static>,
        mut for_each_component: impl FnMut(&mut Self, IntValue<'static>) -> LangResult<()>,
    ) -> LangResult<()> {
        let vec_len = const_uint(vector.get_type().get_size() as u64);
        self.build_iter_loop(
            const_uint(0).into(),
            |c, prev_idx| {
                let prev_idx = prev_idx.into_int_value();
                Ok(c.builder()
                    .build_int_nuw_add(prev_idx, const_uint(1), "nextIterIdx")
                    .into())
            },
            |c, idx| {
                let idx = idx.into_int_value();

                // If idx == vector.len, exit the loop.
                let is_end_of_loop =
                    c.builder()
                        .build_int_compare(IntPredicate::EQ, idx, vec_len, "isEndOfLoop");
                c.build_conditional(
                    is_end_of_loop,
                    |c| Ok(c.build_jump_to_loop_exit().unwrap()),
                    |_| Ok(()),
                )?;

                // Get the component of the vector.
                let component = c
                    .builder()
                    .build_extract_element(vector, idx, "iterVectorComponent")
                    .into_int_value();
                // And pass that to for_each_component().
                for_each_component(c, component)?;

                Ok(())
            },
        )
    }

    /// Builds instructions using the given closure for each cell in a cell
    /// state pattern.
    pub fn build_pattern_iter(
        &mut self,
        pattern: &PatternValue,
        mut for_each_cell: impl FnMut(&mut Self, &[isize], IntValue<'static>) -> LangResult<()>,
    ) -> LangResult<()> {
        for pos in pattern.shape.positions() {
            let pos_vector_value =
                VectorType::const_vector(&pos.iter().map(|&x| const_int(x as i64)).collect_vec());
            self.build_get_pattern_cell_state(
                pattern,
                pos_vector_value,
                // self.value_from_const(ConstValue::Vector(pos)),
                // Cell state is in bounds, as it should be.
                |c, cell_state| for_each_cell(c, &pos, cell_state),
                // Cell state is out of bounds (which should NOT happen here).
                |c| Ok(c.build_return_internal_err()),
            )?;
        }
        Ok(())
    }

    /// Builds instructions using the given closure that run for each integer in
    /// an integer range.
    pub fn build_int_range_iter(
        &mut self,
        range: VectorValue<'static>,
        mut for_each_int: impl FnMut(&mut Self, IntValue<'static>) -> LangResult<()>,
    ) -> LangResult<()> {
        let (start, end, step) = self.build_split_range(range);
        let zero = const_int(0);

        let is_step_zero =
            self.builder()
                .build_int_compare(IntPredicate::EQ, step, zero, "isStepZero");
        let is_step_negative =
            self.builder()
                .build_int_compare(IntPredicate::SLT, step, zero, "isStepNegative");

        self.build_iter_loop(
            start.into(),
            |c, prev_iter_value| {
                // Compute the value for the next iteration based on the
                // previous one. If addition causes overflow, break out of the
                // loop.
                c.build_checked_int_arithmetic(
                    prev_iter_value.into_int_value(),
                    step,
                    "sadd",
                    |c| Ok(c.build_jump_to_loop_exit().unwrap()),
                )
            },
            |c, iter_value| {
                let iter_value = iter_value.into_int_value();

                // If the step is 0, just skip the loop entirely. Infinite loops
                // are bad.
                c.build_conditional(
                    is_step_zero,
                    |c| Ok(c.build_jump_to_loop_exit().unwrap()),
                    |_| Ok(()),
                )?;

                // Decide whether to exit the loop. If step < 0, then exit if
                // iter_value < end. If step > 0, then exit if iter_value > end.
                // (We already handled the case of step == 0.)
                let is_too_low =
                    c.builder()
                        .build_int_compare(IntPredicate::SLT, iter_value, end, "tooLow");
                let is_too_high =
                    c.builder()
                        .build_int_compare(IntPredicate::SGT, iter_value, end, "tooHigh");
                let past_end_value = c
                    .builder()
                    .build_select(is_step_negative, is_too_low, is_too_high, "pastEndOfRange")
                    .into_int_value();
                c.build_conditional(
                    past_end_value,
                    |c| Ok(c.build_jump_to_loop_exit().unwrap()),
                    |_| Ok(()),
                )?;

                // Do the code!
                for_each_int(c, iter_value)?;

                Ok(())
            },
        )?;

        Ok(())
    }

    /// Builds a loop using the given closures.
    fn build_iter_loop(
        &mut self,
        first_iter_value: BasicValueEnum<'static>,
        mut build_compute_next_iter_value: impl FnMut(
            &mut Self,
            BasicValueEnum<'static>,
        ) -> LangResult<BasicValueEnum<'static>>,
        mut build_each_iteration: impl FnMut(&mut Self, BasicValueEnum<'static>) -> LangResult<()>,
    ) -> LangResult<()> {
        // pre_loop (where we are now) jumps straight to main_loop, which starts
        // with a PHI node for the iteration value.
        let pre_loop_bb = self.current_block();
        let main_loop_bb = self.append_basic_block("loopBody");
        self.builder().build_unconditional_branch(main_loop_bb);
        self.builder().position_at_end(main_loop_bb);
        let iter_value_phi = self.builder().build_phi(super::types::int(), "iterValue");
        self.builder().position_at_end(pre_loop_bb);
        let iter_value = iter_value_phi.as_basic_value();
        iter_value_phi.add_incoming(&[(&first_iter_value, self.current_block())]);

        self.enter_loop();

        // At the beginning of each loop iteration (NOT including the first),
        // compute the value for the next iteration based on the previous one.
        let prev_iter_value = iter_value;
        let next_iter_value = build_compute_next_iter_value(self, prev_iter_value)?;
        iter_value_phi.add_incoming(&[(&next_iter_value, self.current_block())]);
        self.builder().build_unconditional_branch(main_loop_bb);

        // Now we're at the part of the loop that's executed for every loop
        // iteration, including the first.
        self.builder().position_at_end(main_loop_bb);

        build_each_iteration(self, iter_value)?;

        if self.needs_terminator() {
            self.build_jump_to_loop_entry().unwrap();
        }

        self.exit_loop();
        Ok(())
    }
}
