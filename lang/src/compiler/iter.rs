use itertools::Itertools;

use inkwell::types::VectorType;
use inkwell::values::{BasicValueEnum, IntValue, StructValue, VectorValue};
use inkwell::IntPredicate;

use super::{const_int, const_uint, Compiler, PatternValue, Value};
use crate::types::LangInt;
use crate::LangResult;

impl Compiler {
    /// Compiles a (possibly unrolled) for loop using the given closure to
    /// generate code for each iteration.
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
            Value::Rectangle(r) => {
                self.build_rectangle_iter(r, |c, pos| compile_for_each(c, Value::Vector(pos)))
            }
            Value::CellStateFilter(state_count, f) => {
                self.build_cell_state_filter_iter(state_count, f, |c, cell_state| {
                    compile_for_each(c, Value::CellState(cell_state))
                })
            }
            _ => internal_error!("Don't know how to iterate over type {}", value.ty()),
        }
    }

    /// Builds a loop over the elements of a vector.
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

    /// Builds a loop over the cells in a cell state pattern.
    pub fn build_pattern_iter(
        &mut self,
        pattern: &PatternValue,
        mut for_each_cell: impl FnMut(&mut Self, &[isize], IntValue<'static>) -> LangResult<()>,
    ) -> LangResult<()> {
        for pos in pattern.shape.positions() {
            let pos_vector_value =
                VectorType::const_vector(&pos.iter().map(|&x| const_int(x as i64)).collect_vec());
            let cell_state = self.build_get_pattern_cell_state(
                pattern,
                pos_vector_value,
                // Cell state is out of bounds (which should NOT happen here).
                |c| Ok(c.build_return_internal_err()),
            )?;
            for_each_cell(self, &pos, cell_state)?;
        }
        Ok(())
    }

    /// Builds a loop over the integers in an integer range.
    pub fn build_int_range_iter(
        &mut self,
        range: VectorValue<'static>,
        mut for_each_int: impl FnMut(&mut Self, IntValue<'static>) -> LangResult<()>,
    ) -> LangResult<()> {
        let (start, end, step) = self.build_split_range(range);

        let is_step_zero =
            self.builder()
                .build_int_compare(IntPredicate::EQ, step, const_int(0), "isStepZero");
        let is_step_negative = self.builder().build_int_compare(
            IntPredicate::SLT,
            step,
            const_int(0),
            "isStepNegative",
        );

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

    /// Builds a loop over the positions in a hyperrectangle.
    pub fn build_rectangle_iter(
        &mut self,
        rect: StructValue<'static>,
        mut for_each_pos: impl FnMut(&mut Self, VectorValue<'static>) -> LangResult<()>,
    ) -> LangResult<()> {
        let (start, end) = self.build_split_rectangle(rect);
        let start_values = self.build_split_vector(start);
        let end_values = self.build_split_vector(end);
        assert_eq!(start_values.len(), end_values.len());
        let ndim = start_values.len();

        // Compute step (+1 or -1) along each axis.
        let step_values = start_values
            .iter()
            .zip(&end_values)
            .map(|(&int_start, &int_end)| self.build_infer_range_step(int_start, int_end))
            .collect_vec();

        self.build_iter_loop(
            start.into(),
            |c, old_pos| {
                // Add a PHI node for the new position.
                let (done_with_increment_bb, phi) = c.append_basic_block_with_phi(
                    "doneWithIncrementPos",
                    start.get_type(),
                    "incrementedPos",
                );

                let mut pos = old_pos.into_vector_value();
                for axis in 0..ndim {
                    // Extract the component.
                    let old_component = c
                        .builder()
                        .build_extract_element(pos, const_uint(axis as u64), "oldPosComponent")
                        .into_int_value();
                    // Check if we've reached the end of this axis.
                    let is_end_of_axis = c.builder().build_int_compare(
                        IntPredicate::EQ,
                        old_component,
                        end_values[axis],
                        "isEndOfAxis",
                    );
                    let mut pos_with_reset_component = pos.clone();
                    c.build_conditional(
                        is_end_of_axis,
                        |c| {
                            // We have, so reset it to its start value and try to
                            // increment the next component.
                            pos_with_reset_component = c.builder().build_insert_element(
                                pos,
                                start_values[axis],
                                const_uint(axis as u64),
                                "posWithResetComponent",
                            );
                            Ok(())
                        },
                        |c| {
                            // We haven't, so increment this component and don't
                            // increment any more.
                            let new_component = c.builder().build_int_nsw_add(
                                old_component,
                                step_values[axis],
                                "incrementedComponent",
                            );
                            let incremented_pos = c.builder().build_insert_element(
                                pos,
                                new_component,
                                const_uint(axis as u64),
                                "posWithIncrementedComponent",
                            );
                            phi.add_incoming(&[(&incremented_pos, c.current_block())]);
                            c.builder()
                                .build_unconditional_branch(done_with_increment_bb);
                            Ok(())
                        },
                    )?;
                    pos = pos_with_reset_component;
                }
                // If we made it this far without jumping to
                // done_with_increment_bb, then every single component has been
                // reset so now we're done iterating.
                c.build_jump_to_loop_exit().unwrap();

                // Well, we're done incrementing the position!
                c.builder().position_at_end(done_with_increment_bb);
                Ok(phi.as_basic_value())
            },
            |c, pos| for_each_pos(c, pos.into_vector_value()),
        )?;

        Ok(())
    }

    /// Builds a loop over all the cell states included by a cell state filter.
    pub fn build_cell_state_filter_iter(
        &mut self,
        state_count: usize,
        filter: VectorValue<'static>,
        mut for_each_cell_state: impl FnMut(&mut Self, IntValue<'static>) -> LangResult<()>,
    ) -> LangResult<()> {
        // Delegate to build_int_range_iter().
        let start = const_int(0);
        let end = const_int(state_count as LangInt - 1);
        let range = self.build_construct_range(start, end, None);
        self.build_int_range_iter(range, |c, i| {
            // Truncate integer to cell state.
            let cell_state =
                c.builder()
                    .build_int_truncate(i, super::types::cell_state(), "filterComponentIdx");
            // Extract the bit.
            let idx = c.build_compute_cell_state_filter_idx(state_count, cell_state)?;
            let int = c
                .builder()
                .build_extract_element(filter, idx.vec_idx, "filterComponent")
                .into_int_value();
            let bit = c.builder().build_and(int, idx.bitmask, "filterBit");
            // Execute the code in the loop if the bit is 1.
            c.build_conditional(bit, |c| for_each_cell_state(c, cell_state), |_| Ok(()))?;
            Ok(())
        })
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
        // The current block jumps straight to main_loop, which starts with a
        // PHI node for the iteration value.
        let (loop_body_bb, iter_value_phi) =
            self.append_basic_block_with_phi("loopBody", first_iter_value.get_type(), "iterValue");
        iter_value_phi.add_incoming(&[(&first_iter_value, self.current_block())]);
        self.builder().build_unconditional_branch(loop_body_bb);
        let iter_value = iter_value_phi.as_basic_value();

        self.enter_loop();
        let loop_reentry_bb = self.current_block();
        self.builder().position_at_end(loop_body_bb);

        // Build the contents of the loop, and compute the value for the next
        // iteration.
        build_each_iteration(self, iter_value)?;
        if self.needs_terminator() {
            self.build_jump_to_loop_entry().unwrap();
        }
        self.builder().position_at_end(loop_reentry_bb);
        let next_iter_value = build_compute_next_iter_value(self, iter_value)?;
        iter_value_phi.add_incoming(&[(&next_iter_value, self.current_block())]);
        self.builder().build_unconditional_branch(loop_body_bb);

        self.exit_loop();
        Ok(())
    }
}
