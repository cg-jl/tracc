use std::collections::HashMap;

use super::{
    BasicBlock, Binding, BlockBinding, ByteSize, Condition, CouldBeConstant, Statement, Value,
};
use crate::error::SourceMetadata;
use crate::grammar::lexer::Source;
use crate::intermediate::{BlockEnd, Branch, PhiDescriptor};
use crate::output::Output;
use crate::output_impl_From;
use crate::{ast, error};
use thiserror::Error;

#[derive(Default)]
struct IRGenState {
    blocks: Vec<BasicBlock>,
    latest_var: usize,
}

impl IRGenState {
    fn next_binding(&mut self) -> Binding {
        let v = self.latest_var;
        self.latest_var += 1;
        Binding(v)
    }

    // a binding is created once the block has finished
    fn push_block(&mut self, block: BasicBlock) -> BlockBinding {
        // note: joining blocks so that they have always an end mark is done after everything has
        // been generated
        self.blocks.push(block);
        BlockBinding(self.blocks.len() - 1)
    }
    fn get_mut(&mut self, BlockBinding(index): BlockBinding) -> &mut BasicBlock {
        // note: index is safe because it can only be generated through `push_block`
        &mut self.blocks[index]
    }
}

// to know the variables we need another structure that knows all the allocations
type VariableMemories<'code> = HashMap<&'code str, (Binding, ByteSize)>;

#[derive(Error, Debug)]
pub enum IRGenError {
    #[error("unknown variable: {0:?}")]
    UnknownVariable(String),
}

// type BlockOutput = Output<super::Statement>;

fn expr_as_ptr<'code>(
    state: &mut IRGenState,
    target: Binding,
    mem_bindings: &VariableMemories<'code>,
    expr: ast::Expr<'code>,
    source_meta: &SourceMetadata<'code>,
) -> Result<ByteSize, error::Error<IRGenError>> {
    match expr {
        ast::Expr::Variable {
            name: Source { source: name, span },
        } => {
            let (mem_binding, byte_size) = *mem_bindings.get(name).ok_or_else(|| {
                error::Error::new(IRGenError::UnknownVariable(name.to_string()))
                    .with_source(span, source_meta)
            })?;
            state.push_block(BasicBlock {
                statements: vec![Statement::Assign {
                    index: target,
                    value: Value::Binding(mem_binding),
                }],
                end: None,
            });
            Ok(byte_size)
        }
        _ => unreachable!("pointers are not valid yet"),
    }
}

fn generate_expr<'code>(
    state: &mut IRGenState,
    target: Binding,
    mem_bindings: &VariableMemories<'code>,
    expr: ast::Expr<'code>,
    source_meta: &SourceMetadata<'code>,
) -> Result<BlockBinding, error::Error<IRGenError>> {
    match expr {
        ast::Expr::Variable {
            name: Source { source: name, .. },
        } => {
            let (mem_binding, load_size) = *mem_bindings
                .get(name)
                .ok_or_else(|| IRGenError::UnknownVariable(name.to_string()))
                .map_err(error::Error::new)?;
            let assign = Statement::Assign {
                index: target,
                value: Value::Load {
                    byte_size: load_size,
                    mem_binding,
                },
            };
            Ok(state.push_block(BasicBlock {
                statements: vec![assign],
                end: None,
            }))
        }
        ast::Expr::Constant(constant) => Ok(state.push_block(BasicBlock {
            statements: vec![Statement::Assign {
                index: target,
                value: Value::Constant(constant as u64),
            }],
            end: None,
        })),
        ast::Expr::Unary {
            operator,
            expr: (expr, span),
        } => {
            let calculated_expr = state.next_binding();
            generate_expr(state, calculated_expr, mem_bindings, *expr, source_meta)
                .map_err(|e| e.with_backup_source(span, source_meta))?;
            let applied_value = state.next_binding();
            let op = match operator {
                ast::UnaryOp::Negate => Value::Negate {
                    binding: calculated_expr,
                },
                ast::UnaryOp::BitNot => Value::FlipBits {
                    binding: calculated_expr,
                },
                ast::UnaryOp::LogicNot => Value::Cmp {
                    condition: Condition::NotEquals,
                    lhs: calculated_expr,
                    rhs: CouldBeConstant::Constant(0),
                },
            };
            let stmt = Statement::Assign {
                index: applied_value,
                value: op,
            };
            let bb = BasicBlock {
                statements: vec![stmt],
                end: None,
            };
            Ok(state.push_block(bb))
        }
        ast::Expr::Binary {
            operator,
            lhs: (lhs_expr, lhs_span),
            rhs: (rhs_expr, rhs_span),
        } => {
            match operator {
                ast::BinaryOp::Logic(logic_op) => {
                    let lhs_binding = state.next_binding();
                    let rhs_binding = state.next_binding();
                    let bailout_result = state.next_binding();
                    let lhs_block =
                        generate_expr(state, lhs_binding, mem_bindings, *lhs_expr, source_meta)
                            .map_err(|e| e.with_backup_source(lhs_span, source_meta))?;
                    let rhs_block =
                        generate_expr(state, rhs_binding, mem_bindings, *rhs_expr, source_meta)
                            .map_err(|e| e.with_backup_source(rhs_span, source_meta))?;
                    let bailout_block = state.push_block(BasicBlock::default());
                    let end_block = state.push_block(BasicBlock {
                        statements: vec![Statement::Assign {
                            index: target,
                            value: Value::Phi {
                                nodes: vec![
                                    PhiDescriptor {
                                        variable: bailout_result,
                                        block_from: bailout_block,
                                    },
                                    PhiDescriptor {
                                        variable: rhs_binding,
                                        block_from: rhs_block,
                                    },
                                ],
                            },
                        }],
                        end: None,
                    });
                    // add the bailout mechanism
                    {
                        // complete the bailout block
                        let bailout_bb = state.get_mut(bailout_block);
                        bailout_bb.statements.push(Statement::Assign {
                            index: bailout_result,
                            value: Value::Constant(0),
                        });
                        bailout_bb.end = Some(BlockEnd::Branch(Branch::Unconditional {
                            target: end_block,
                        }));
                    }
                    {
                        // make the lhs block end after the computations
                        let bail_condition = match logic_op {
                            ast::LogicOp::And => Condition::Equals,
                            ast::LogicOp::Or => Condition::NotEquals,
                        };
                        let bail_value = Value::Cmp {
                            condition: bail_condition,
                            lhs: lhs_binding,
                            rhs: CouldBeConstant::Constant(0),
                        };
                        let flag = state.next_binding();
                        let lhs_bb = state.get_mut(lhs_block);
                        assert!(lhs_bb.end.is_none());
                        lhs_bb.statements.push(Statement::Assign {
                            index: flag,
                            value: bail_value,
                        });
                        lhs_bb.end = Some(BlockEnd::Branch(Branch::Conditional {
                            flag,
                            target_false: rhs_block,
                            target_true: bailout_block,
                        }));
                    }
                    todo!()
                }
                ast::BinaryOp::Assignment { op } => {
                    // compute the value (this might modify the lhs memory so we want to do it
                    // first)
                    let rhs = state.next_binding();
                    generate_expr(state, rhs, mem_bindings, *rhs_expr, source_meta)
                        .map_err(|e| e.with_backup_source(rhs_span, source_meta))?;

                    // we want to get the pointer
                    let lhs_mem = state.next_binding();
                    let lhs_mem_size =
                        expr_as_ptr(state, lhs_mem, mem_bindings, *lhs_expr, source_meta)
                            .map_err(|e| e.with_backup_source(lhs_span, source_meta))?;

                    let mut statements = Vec::new();
                    let compute_value = if let Some(assignment_enabled) = op {
                        // if we have a binary op, we have to read again the memory
                        let lhs_read = state.next_binding();
                        statements.push(Statement::Assign {
                            index: lhs_read,
                            value: Value::Load {
                                mem_binding: lhs_mem,
                                byte_size: lhs_mem_size,
                            },
                        });
                        // then compute the binary op
                        compute_assignment_enabled(
                            assignment_enabled,
                            lhs_read,
                            rhs,
                            &mut statements,
                            state,
                        )
                    } else {
                        Value::Binding(rhs)
                    };
                    // set the target to the value we computed
                    statements.push(Statement::Assign {
                        index: target,
                        value: compute_value,
                    });
                    // store the target into memory
                    statements.push(Statement::Store {
                        binding: target,
                        mem_binding: lhs_mem,
                        byte_size: lhs_mem_size,
                    });
                    Ok(state.push_block(BasicBlock {
                        statements,
                        end: None,
                    }))
                }
                _ => {
                    let lhs = state.next_binding();
                    let rhs = state.next_binding();
                    // compute lhs then rhs
                    generate_expr(state, lhs, mem_bindings, *lhs_expr, source_meta)
                        .map_err(|e| e.with_backup_source(lhs_span, source_meta))?;
                    generate_expr(state, rhs, mem_bindings, *rhs_expr, source_meta)
                        .map_err(|e| e.with_backup_source(rhs_span, source_meta))?;
                    // then join them together
                    let mut statements = Vec::new();
                    let result_value = match operator {
                        ast::BinaryOp::Logic(_) | ast::BinaryOp::Assignment { .. } => {
                            unreachable!()
                        }
                        ast::BinaryOp::Relational(relation) => Value::Cmp {
                            condition: relation.to_condition(),
                            lhs,
                            rhs: CouldBeConstant::Binding(rhs),
                        },
                        ast::BinaryOp::Bit(bitop) => compute_bitop(bitop, lhs, rhs),
                        ast::BinaryOp::Arithmetic(arithm) => {
                            compute_arithmetic(arithm, lhs, rhs, &mut statements, state)
                        }
                    };
                    statements.push(Statement::Assign {
                        index: target,
                        value: result_value,
                    });
                    Ok(state.push_block(BasicBlock {
                        statements,
                        end: None,
                    }))
                }
            }
        }
    }
}
fn compute_arithmetic(
    op: ast::ArithmeticOp,
    lhs: Binding,
    rhs: Binding,
    statements: &mut Vec<Statement>,
    state: &mut IRGenState,
) -> Value {
    match op {
        ast::ArithmeticOp::Add => Value::Add {
            lhs,
            rhs: CouldBeConstant::Binding(rhs),
        },
        ast::ArithmeticOp::Subtract => Value::Subtract {
            lhs,
            rhs: CouldBeConstant::Binding(rhs),
        },
        ast::ArithmeticOp::Multiply => Value::Multiply {
            lhs,
            rhs: CouldBeConstant::Binding(rhs),
        },
        ast::ArithmeticOp::Divide => Value::Divide {
            lhs,
            rhs: CouldBeConstant::Binding(rhs),
            is_signed: true, // TODO: checking unsigned types in expression results
        },
        ast::ArithmeticOp::Modulo => {
            // we'll have q = lhs / rhs, then qr = q * rhs,
            // then r = lhs - qr
            let quotient = state.next_binding();
            let quotient_times_rhs = state.next_binding();
            statements.push(Statement::Assign {
                index: quotient,
                value: Value::Divide {
                    lhs,
                    rhs: CouldBeConstant::Binding(rhs),
                    is_signed: false, // NOTE: should be this signedness be product of lhs's or rhs's type? or we keep it to unsigned? i.e if one of them is signed, we want signed otherwise unsigned is fine?
                },
            });
            statements.push(Statement::Assign {
                index: quotient_times_rhs,
                value: Value::Multiply {
                    lhs: quotient,
                    rhs: CouldBeConstant::Binding(rhs),
                },
            });
            Value::Subtract {
                lhs,
                rhs: CouldBeConstant::Binding(quotient_times_rhs),
            }
        }
    }
}
fn compute_bitop(bitop: ast::BitOp, lhs: Binding, rhs: Binding) -> Value {
    match bitop {
        ast::BitOp::And => Value::And {
            lhs,
            rhs: CouldBeConstant::Binding(rhs),
        },
        ast::BitOp::Or => Value::Or {
            lhs,
            rhs: CouldBeConstant::Binding(rhs),
        },
        ast::BitOp::Xor => Value::Xor {
            lhs,
            rhs: CouldBeConstant::Binding(rhs),
        },
        ast::BitOp::RightShift => Value::Lsr {
            lhs,
            rhs: CouldBeConstant::Binding(rhs),
        },
        ast::BitOp::LeftShift => Value::Lsl {
            lhs,
            rhs: CouldBeConstant::Binding(rhs),
        },
    }
}
fn compute_assignment_enabled(
    op: ast::AssignmentEnabledOp,
    lhs: Binding,
    rhs: Binding,
    statements: &mut Vec<Statement>,
    state: &mut IRGenState,
) -> Value {
    match op {
        ast::AssignmentEnabledOp::Arithmetic(arithm) => {
            compute_arithmetic(arithm, lhs, rhs, statements, state)
        }
        ast::AssignmentEnabledOp::Bit(bitop) => compute_bitop(bitop, lhs, rhs),
    }
}
