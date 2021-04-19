
                // Boolean operators 'and' and 'or' short-circuit, so we have to
                // handle them specially.
                LogicalAnd => {
                    let ret =
                        runtime.eval_expr(lhs)?.to_bool()? && runtime.eval_expr(rhs)?.to_bool()?;
                    return Ok(Value::Integer(ret as LangInt));
                }
                LogicalOr => {
                    let ret =
                        runtime.eval_expr(lhs)?.to_bool()? || runtime.eval_expr(rhs)?.to_bool()?;
                    return Ok(Value::Integer(ret as LangInt));
                }
                LogicalXor => {
                    let ret =
                        runtime.eval_expr(lhs)?.to_bool()? != runtime.eval_expr(rhs)?.to_bool()?;
                    return Ok(Value::Integer(ret as LangInt));
                }
