use super::*;
use crate::ast;

pub fn compile_expr<'code>(
    state: &mut IRGenState,
    mut builder: BlockBuilder,
    target: Binding,
    expr: ast::Expr<'code>,
    bindings: &mut BindingCounter,
    variables: &VariableTracker<'code>,
    source_info: &SourceMetadata<'code>,
) -> Result<BlockBuilder, VarE> {
    match expr {
        // TODO: use a different strategy if the type is a structure
        ast::Expr::Variable {
            name: Source { source: name, .. },
        } => {
            let (variable_mem, variable_size) = *variables
                .get(name)
                .ok_or_else(|| VarE::new(VarError::UnknownVariable(name.to_string())))?;
            builder.load(target, variable_mem, variable_size);
            Ok(builder) // give back the same block builder since we didn't have to finish the block
        }
        ast::Expr::Constant(constant) => {
            builder.assign(target, constant as u64);
            Ok(builder)
        }
        ast::Expr::Unary {
            operator,
            expr: (expr, expr_span),
        } => {
            let expr_target = bindings.next_binding();
            let mut end = compile_expr(
                state,
                builder,
                expr_target,
                *expr,
                bindings,
                variables,
                source_info,
            )
            .map_err(|e| e.with_backup_source(expr_span, source_info))?;
            end.assign(
                target,
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
            );

            Ok(end)
        }
        ast::Expr::Binary {
            operator,
            lhs: (lhs_expr, lhs_span),
            rhs: (rhs_expr, rhs_span),
        } => match operator {
            ast::BinaryOp::Arithmetic(arithmop) => {
                let lhs = bindings.next_binding();
                let rhs = bindings.next_binding();
                // compute first lhs, then rhs
                let builder = compile_expr(
                    state,
                    builder,
                    lhs,
                    *lhs_expr,
                    bindings,
                    variables,
                    source_info,
                )
                .map_err(|e| e.with_backup_source(lhs_span, source_info))?;
                let mut builder = compile_expr(
                    state,
                    builder,
                    rhs,
                    *rhs_expr,
                    bindings,
                    variables,
                    source_info,
                )
                .map_err(|e| e.with_backup_source(rhs_span, source_info))?;
                compile_arithmetic(&mut builder, bindings, arithmop, target, lhs, rhs);
                Ok(builder)
            }
            ast::BinaryOp::Bit(bitop) => {
                let lhs = bindings.next_binding();
                let rhs = bindings.next_binding();
                // compute first lhs, then rhs
                let builder = compile_expr(
                    state,
                    builder,
                    lhs,
                    *lhs_expr,
                    bindings,
                    variables,
                    source_info,
                )
                .map_err(|e| e.with_backup_source(lhs_span, source_info))?;
                let mut builder = compile_expr(
                    state,
                    builder,
                    rhs,
                    *rhs_expr,
                    bindings,
                    variables,
                    source_info,
                )
                .map_err(|e| e.with_backup_source(rhs_span, source_info))?;
                compile_bitop(&mut builder, bitop, target, lhs, rhs);
                Ok(builder)
            }
            ast::BinaryOp::Relational(relational) => {
                let lhs = bindings.next_binding();
                let rhs = bindings.next_binding();
                // compute first lhs, then rhs
                let builder = compile_expr(
                    state,
                    builder,
                    lhs,
                    *lhs_expr,
                    bindings,
                    variables,
                    source_info,
                )
                .map_err(|e| e.with_backup_source(lhs_span, source_info))?;
                let mut builder = compile_expr(
                    state,
                    builder,
                    rhs,
                    *rhs_expr,
                    bindings,
                    variables,
                    source_info,
                )
                .map_err(|e| e.with_backup_source(rhs_span, source_info))?;
                builder.assign(target, relational_as_value(relational, lhs, rhs));
                Ok(builder)
            }
            ast::BinaryOp::Logic(logicop) => {
                // lhs is going to be computed straight ahead
                let lhs = bindings.next_binding();
                let rhs = bindings.next_binding();
                let mut lhs_builder = compile_expr(
                    state,
                    builder,
                    lhs,
                    *lhs_expr,
                    bindings,
                    variables,
                    source_info,
                )
                .map_err(|e| e.with_backup_source(lhs_span, source_info))?;
                let rhs_builder = {
                    let block = state.new_block();
                    compile_expr(
                        state,
                        block,
                        rhs,
                        *rhs_expr,
                        bindings,
                        variables,
                        source_info,
                    )
                    .map_err(|e| e.with_backup_source(rhs_span, source_info))
                }?;
                // create the new block that will start with a phi node to merge the two branches
                let mut end_builder = state.new_block();
                end_builder.assign(
                    target,
                    Value::Phi {
                        nodes: vec![
                            PhiDescriptor {
                                value: lhs.into(),
                                block_from: lhs_builder.block(),
                            },
                            PhiDescriptor {
                                value: rhs.into(),
                                block_from: rhs_builder.block(),
                            },
                        ],
                    },
                );
                // finish rhs by telling it to jump directly to the end
                let compute_rhs = rhs_builder.finish_block(
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
                        lhs: flag,
                        rhs: 0.into(),
                    },
                );
                lhs_builder.finish_block(
                    state,
                    Branch::Conditional {
                        flag,
                        target_true: end_builder.block(),
                        target_false: compute_rhs,
                    },
                );

                Ok(end_builder)
            }
            ast::BinaryOp::Assignment { op } => {
                let rhs = bindings.next_binding();
                // compute rhs
                let mut builder = compile_expr(
                    state,
                    builder,
                    rhs,
                    *rhs_expr,
                    bindings,
                    variables,
                    source_info,
                )
                .map_err(|e| e.with_backup_source(rhs_span, source_info))?;
                let (lhs_mem, lhs_size) = expr_as_ptr(*lhs_expr, variables)
                    .map_err(|e| e.with_backup_source(lhs_span, source_info))?;
                if let Some(assignment_enabled) = op {
                    // 1. read the memory
                    let lhs = bindings.next_binding();
                    builder.load(lhs, lhs_mem, lhs_size);
                    // 2. Compute the value
                    match assignment_enabled {
                        ast::AssignmentEnabledOp::Arithmetic(arithmop) => {
                            compile_arithmetic(&mut builder, bindings, arithmop, target, lhs, rhs)
                        }
                        ast::AssignmentEnabledOp::Bit(bitop) => {
                            compile_bitop(&mut builder, bitop, target, lhs, rhs)
                        }
                    }
                } else {
                    builder.assign(target, rhs)
                }
                Ok(builder)
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
    target: Binding,
    lhs: Binding,
    rhs: Binding,
) {
    match arithmop {
        ast::ArithmeticOp::Add => builder.assign(
            target,
            Value::Add {
                lhs,
                rhs: rhs.into(),
            },
        ),
        ast::ArithmeticOp::Subtract => builder.assign(
            target,
            Value::Add {
                lhs,
                rhs: rhs.into(),
            },
        ),
        ast::ArithmeticOp::Multiply => builder.assign(
            target,
            Value::Multiply {
                lhs,
                rhs: rhs.into(),
            },
        ),
        ast::ArithmeticOp::Divide => builder.assign(
            target,
            Value::Divide {
                lhs,
                rhs: rhs.into(),
                is_signed: true, // assume signed division for now (i32)
            },
        ),
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
            builder.assign(
                target,
                Value::Subtract {
                    lhs,
                    rhs: qxd.into(),
                },
            )
        }
    }
}

// bit operations can't go out of the block, and
// require both elements to be computed first
fn compile_bitop(
    builder: &mut BlockBuilder,
    bitop: ast::BitOp,
    target: Binding,
    lhs: Binding,
    rhs: Binding,
) {
    builder.assign(
        target,
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
        },
    )
}
