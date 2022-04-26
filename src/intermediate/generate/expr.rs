use super::{
    statement, Binding, BindingCounter, BlockBuilder, Branch, ByteSize, Condition, IRGenState,
    PhiDescriptor, Source, SourceMetadata, Value, VarE, VarError, VariableTracker,
};
use crate::ast;

// TODO: consider refactoring logic expressions to use `merge_branches` or even a new utility that
// spits out a phi node (from ternary expression).
// XXX: consider moving from `Value` to `Binding` due to codegen not having to make any
// optimization decisions
pub fn compile_expr<'code>(
    state: &mut IRGenState,
    builder: BlockBuilder,
    expr: ast::Expr<'code>,
    bindings: &mut BindingCounter,
    variables: &VariableTracker<'code>,
    source_info: &SourceMetadata<'code>,
) -> Result<(BlockBuilder, Value), VarE> {
    match expr {
        // TODO: use a different strategy if the type is a structure
        ast::Expr::Variable {
            name: Source { source: name, .. },
        } => {
            let (variable_mem, variable_size) = *variables
                .get(name)
                .ok_or_else(|| VarE::new(VarError::UnknownVariable(name.to_string())))?;
            Ok((
                builder,
                Value::Load {
                    mem_binding: variable_mem,
                    byte_size: variable_size,
                },
            ))
        }
        ast::Expr::Constant(constant) => Ok((builder, Value::Constant(constant))),
        ast::Expr::Ternary {
            condition: (condition_expr, condition_span),
            value_true: (true_expr, true_span),
            value_false: (false_expr, false_span),
        } => {
            let (compute_condition, cond_flag) = {
                let (mut compute, cond_value) = compile_expr(
                    state,
                    builder, // continue the builder we had previously
                    *condition_expr,
                    bindings,
                    variables,
                    source_info,
                )
                .map_err(|e| e.with_backup_source(condition_span, source_info))?;
                let value_binding = bindings.next_binding();
                compute.assign(value_binding, cond_value);
                (compute, value_binding)
            };

            let (compute_if_true, true_binding) = {
                let builder = state.new_block();
                let (mut compute, expr_value) =
                    compile_expr(state, builder, *true_expr, bindings, variables, source_info)
                        .map_err(|e| e.with_backup_source(true_span, source_info))?;
                let value_binding = bindings.next_binding();
                compute.assign(value_binding, expr_value);
                (compute, value_binding)
            };

            let (compute_if_false, false_binding) = {
                let builder = state.new_block();
                let (mut compute, expr_value) = compile_expr(
                    state,
                    builder,
                    *false_expr,
                    bindings,
                    variables,
                    source_info,
                )
                .map_err(|e| e.with_backup_source(false_span, source_info))?;
                let value_binding = bindings.next_binding();
                compute.assign(value_binding, expr_value);
                (compute, value_binding)
            };

            let true_block = compute_if_true.block();
            let false_block = compute_if_false.block();

            Ok((
                statement::merge_branches(
                    state,
                    compute_condition,
                    cond_flag,
                    compute_if_true,
                    compute_if_false,
                ),
                Value::Phi {
                    nodes: vec![
                        PhiDescriptor {
                            value: true_binding,
                            block_from: true_block,
                        },
                        PhiDescriptor {
                            value: false_binding,
                            block_from: false_block,
                        },
                    ],
                },
            ))
        }
        ast::Expr::Unary {
            operator,
            expr: (expr, expr_span),
        } => {
            let expr_target = bindings.next_binding();
            let (mut end, expr_value) =
                compile_expr(state, builder, *expr, bindings, variables, source_info)
                    .map_err(|e| e.with_backup_source(expr_span, source_info))?;
            end.assign(expr_target, expr_value);
            Ok((
                end,
                match operator {
                    ast::UnaryOp::Negate => Value::Negate {
                        binding: expr_target,
                    },
                    ast::UnaryOp::BitNot => Value::FlipBits {
                        binding: expr_target,
                    },
                    ast::UnaryOp::LogicNot => Value::Cmp {
                        condition: Condition::Equals,
                        lhs: expr_target,
                        rhs: 0.into(),
                    },
                },
            ))
        }
        ast::Expr::Binary {
            operator,
            lhs: (lhs_expr, lhs_span),
            rhs: (rhs_expr, rhs_span),
        } => match operator {
            ast::BinaryOp::Arithmetic(arithmop) => {
                // compute first lhs, then rhs
                let (mut builder, lhs_result) =
                    compile_expr(state, builder, *lhs_expr, bindings, variables, source_info)
                        .map_err(|e| e.with_backup_source(lhs_span, source_info))?;
                let lhs = bindings.next_binding();
                builder.assign(lhs, lhs_result);

                let (mut builder, rhs_result) =
                    compile_expr(state, builder, *rhs_expr, bindings, variables, source_info)
                        .map_err(|e| e.with_backup_source(rhs_span, source_info))?;
                let rhs = bindings.next_binding();
                builder.assign(rhs, rhs_result);
                let result = compile_arithmetic(&mut builder, bindings, arithmop, lhs, rhs);
                Ok((builder, result))
            }
            ast::BinaryOp::Bit(bitop) => {
                // compute first lhs, then rhs
                let (mut builder, lhs_result) =
                    compile_expr(state, builder, *lhs_expr, bindings, variables, source_info)
                        .map_err(|e| e.with_backup_source(lhs_span, source_info))?;
                let lhs = bindings.next_binding();
                builder.assign(lhs, lhs_result);

                let (mut builder, rhs_result) =
                    compile_expr(state, builder, *rhs_expr, bindings, variables, source_info)
                        .map_err(|e| e.with_backup_source(rhs_span, source_info))?;

                let rhs = bindings.next_binding();
                builder.assign(rhs, rhs_result);

                let result = compile_bitop(bitop, lhs, rhs);
                Ok((builder, result))
            }
            ast::BinaryOp::Relational(relational) => {
                // compute first lhs, then rhs
                let (mut builder, lhs_result) =
                    compile_expr(state, builder, *lhs_expr, bindings, variables, source_info)
                        .map_err(|e| e.with_backup_source(lhs_span, source_info))?;

                let lhs = bindings.next_binding();
                builder.assign(lhs, lhs_result);

                let (mut builder, rhs_result) =
                    compile_expr(state, builder, *rhs_expr, bindings, variables, source_info)
                        .map_err(|e| e.with_backup_source(rhs_span, source_info))?;
                let rhs = bindings.next_binding();
                builder.assign(rhs, rhs_result);
                let result = relational_as_value(relational, lhs, rhs);
                Ok((builder, result))
            }
            ast::BinaryOp::Logic(logicop) => {
                // lhs is going to be computed straight ahead
                let (mut lhs_builder, lhs_result) =
                    compile_expr(state, builder, *lhs_expr, bindings, variables, source_info)
                        .map_err(|e| e.with_backup_source(lhs_span, source_info))?;

                let lhs = bindings.next_binding();
                lhs_builder.assign(lhs, lhs_result);

                // compile another block in which rhs is computed
                let (rhs_builder, compute_rhs, rhs) = {
                    let block = state.new_block();
                    let start = block.block(); // make sure that lhs jumps to the *start* of rhs's computation
                    let (mut rhs_block, rhs_value) =
                        compile_expr(state, block, *rhs_expr, bindings, variables, source_info)
                            .map_err(|e| e.with_backup_source(rhs_span, source_info))?;
                    let rhs = bindings.next_binding();
                    rhs_block.assign(rhs, rhs_value);

                    (rhs_block, start, rhs)
                };
                // create the new block that will start with a phi node to merge the two branches
                let mut end_builder = state.new_block();
                // finish rhs by telling it to jump directly to the end
                let rhs_block = rhs_builder.finish_block(
                    state,
                    Branch::Unconditional {
                        target: end_builder.block(),
                    },
                );
                // finish lhs by adding the comparison to zero and the conditional branch
                let bail_condition = match logicop {
                    ast::LogicOp::And => Condition::Equals,
                    ast::LogicOp::Or => Condition::NotEquals,
                };
                let flag = bindings.next_binding();
                lhs_builder.assign(
                    flag,
                    Value::Cmp {
                        condition: bail_condition,
                        lhs,
                        rhs: 0.into(),
                    },
                );
                let lhs_block = lhs_builder.finish_block(
                    state,
                    Branch::Conditional {
                        flag,
                        target_true: end_builder.block(),
                        target_false: compute_rhs,
                    },
                );

                let end = bindings.next_binding();
                end_builder.assign(
                    end,
                    Value::Phi {
                        nodes: vec![
                            PhiDescriptor {
                                value: lhs,
                                block_from: lhs_block,
                            },
                            PhiDescriptor {
                                value: rhs,
                                block_from: rhs_block,
                            },
                        ],
                    },
                );

                Ok((
                    end_builder,
                    Value::And {
                        lhs: end,
                        rhs: (1).into(),
                    },
                ))
            }
            ast::BinaryOp::Assignment { op } => {
                // compute rhs
                let (mut builder, rhs_value) =
                    compile_expr(state, builder, *rhs_expr, bindings, variables, source_info)
                        .map_err(|e| e.with_backup_source(rhs_span, source_info))?;

                let rhs = bindings.next_binding();
                builder.assign(rhs, rhs_value);

                let (lhs_mem, lhs_size) = expr_as_ptr(*lhs_expr, variables)
                    .map_err(|e| e.with_backup_source(lhs_span, source_info))?;
                let result_binding = if let Some(assignment_enabled) = op {
                    // 1. read the memory
                    let lhs = bindings.next_binding();
                    builder.load(lhs, lhs_mem, lhs_size);
                    // 2. Compute the value
                    let value = match assignment_enabled {
                        ast::AssignmentEnabledOp::Arithmetic(arithmop) => {
                            compile_arithmetic(&mut builder, bindings, arithmop, lhs, rhs)
                        }
                        ast::AssignmentEnabledOp::Bit(bitop) => compile_bitop(bitop, lhs, rhs),
                    };
                    let result = bindings.next_binding();
                    builder.assign(result, value);
                    result
                } else {
                    rhs
                };
                builder.store(result_binding, lhs_mem, lhs_size);
                Ok((builder, Value::Binding(result_binding)))
            }
        },
    }
}

fn expr_as_ptr<'code>(
    expr: ast::Expr<'code>,
    variables: &VariableTracker<'code>,
) -> Result<(Binding, ByteSize), VarE> {
    match expr {
        ast::Expr::Variable {
            name: Source { source: name, .. },
        } => variables
            .get(name)
            .ok_or_else(|| VarE::new(VarError::UnknownVariable(name.to_string())))
            .copied(),

        _ => unreachable!("pointers are not yet supported!"),
    }
}

fn relational_as_value(relational: ast::Relational, lhs: Binding, rhs: Binding) -> Value {
    Value::Cmp {
        condition: relational.to_condition(),
        lhs,
        rhs: rhs.into(),
    }
}

// arithmetic operations might need to assign more bindings,
// and require both elements to be computed first
fn compile_arithmetic(
    builder: &mut BlockBuilder,
    bindings: &mut BindingCounter,
    arithmop: ast::ArithmeticOp,
    lhs: Binding,
    rhs: Binding,
) -> Value {
    match arithmop {
        ast::ArithmeticOp::Add => Value::Add {
            lhs,
            rhs: rhs.into(),
        },
        ast::ArithmeticOp::Subtract => Value::Subtract {
            lhs,
            rhs: rhs.into(),
        },
        ast::ArithmeticOp::Multiply => Value::Multiply {
            lhs,
            rhs: rhs.into(),
        },
        ast::ArithmeticOp::Divide => Value::Divide {
            lhs,
            rhs: rhs.into(),
            is_signed: true, // assume signed division for now (i32)
        },
        ast::ArithmeticOp::Modulo => {
            // modulo is a bit more complex.
            // It does q = lhs / rhs, qxd = q * rhs, target = lhs - qxd
            // where q = quotient, d = divisor (rhs), qxd = quotient times divisor
            let q = bindings.next_binding();
            let qxd = bindings.next_binding();
            builder.assign(
                q,
                Value::Divide {
                    lhs,
                    rhs: rhs.into(),
                    is_signed: false, // TODO: make signedness on div/modulo depend on signedness of operands
                },
            );
            builder.assign(
                qxd,
                Value::Multiply {
                    lhs: q,
                    rhs: rhs.into(),
                },
            );
            Value::Subtract {
                lhs,
                rhs: qxd.into(),
            }
        }
    }
}

// bit operations can't go out of the block, and
// require both elements to be computed first
fn compile_bitop(bitop: ast::BitOp, lhs: Binding, rhs: Binding) -> Value {
    match bitop {
        ast::BitOp::And => Value::And {
            lhs,
            rhs: rhs.into(),
        },
        ast::BitOp::Or => Value::Or {
            lhs,
            rhs: rhs.into(),
        },
        ast::BitOp::Xor => Value::Xor {
            lhs,
            rhs: rhs.into(),
        },
        ast::BitOp::RightShift => Value::Lsr {
            lhs,
            rhs: rhs.into(),
        },
        ast::BitOp::LeftShift => Value::Lsl {
            lhs,
            rhs: rhs.into(),
        },
    }
}
