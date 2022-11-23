use super::*;
use crate::ast;

#[derive(Debug, Clone, Copy)]
pub struct LoopStatus {
    break_point: BlockBinding,
    continue_point: BlockBinding,
}

pub fn compile_statement<'code>(
    state: &mut IRGenState,
    mut builder: BlockBuilder,
    bindings: &mut BindingCounter,
    statement: ast::Statement<'code>,
    variables: &mut VariableTracker<'code>,
    functions: &HashMap<&'code str, BlockBinding>,
    current_loop: Option<LoopStatus>,
    block_depth: usize,
    source_meta: &SourceMetadata,
) -> Result<BlockBuilder, StE> {
    match statement {
        ast::Statement::Loop {
            condition: (condition, condition_span),
            body: (body, body_span),
            kind,
        } => {
            let condition_block = state.new_block();
            let body_block = state.new_block();
            let exit = state.new_block();

            let condition_entrypoint = condition_block.block();
            let body_entrypoint = body_block.block();

            // a do while executes the body at least once, while the 'while' loop first checks
            // the condition.
            let loop_entrypoint = if let ast::LoopKind::DoWhile = kind {
                body_entrypoint
            } else {
                condition_entrypoint
            };

            // finish the current block by making it jump to our loop entrypoint
            builder.finish_block(
                state,
                Branch::Unconditional {
                    target: loop_entrypoint,
                },
            );

            // compile the condition
            let (mut condition_block, condition_value) = expr::compile_expr(
                state,
                condition_block,
                condition,
                bindings,
                variables,
                functions,
                block_depth,
                source_meta,
            )
            .map_err(|e| {
                e.with_backup_source(condition_span, source_meta)
                    .map_kind(From::from)
            })?;
            // assign it to a binding
            let condition_binding = bindings.next_binding();
            condition_block.assign(condition_binding, condition_value);
            // ...and redirect it conditionally either to the body or the end
            condition_block.finish_block(
                state,
                Branch::Conditional {
                    flag: condition_binding,
                    target_true: body_entrypoint,
                    target_false: exit.block(),
                },
            );

            // compile the last thing and link it if necessary
            let continue_point = if let ast::LoopKind::For {
                on_iteration_end: (stmt, span),
            } = kind
            {
                let end_block = state.new_block();
                let continue_point = end_block.block();
                compile_statement(
                    state,
                    end_block,
                    bindings,
                    *stmt,
                    variables,
                    functions,
                    current_loop,
                    block_depth,
                    source_meta,
                )?
                .finish_block(
                    state,
                    BlockEnd::Branch(Branch::Unconditional {
                        target: condition_entrypoint,
                    }),
                );
                continue_point
            } else {
                condition_entrypoint
            };

            // compile the body
            let mut body = compile_statement(
                state,
                body_block,
                bindings,
                *body,
                variables,
                functions,
                Some(LoopStatus {
                    continue_point,
                    break_point: exit.block(),
                }),
                block_depth,
                source_meta,
            )
            .map_err(|e| e.with_backup_source(body_span, source_meta))?;

            // the loop body always 'ends' at the condition to know whether to continue or not.
            body.finish_block(
                state,
                Branch::Unconditional {
                    target: continue_point,
                },
            );

            // we end at the exit point
            Ok(exit)
        }
        ast::Statement::LoopBreak => {
            let loop_status =
                current_loop.ok_or_else(|| StE::new(StatementError::UnwantedBreak))?;
            // make another block so everything continues normally, although it's going to
            // get removed afterwards.
            // TODO: make this return control flow so that I can make it stop processing when it
            // finds a break/continue (and forward the breaking if we aren't hitting any
            // branches/loop)
            let unused_block = state.new_block();

            builder.finish_block(
                state,
                BlockEnd::Branch(Branch::Unconditional {
                    target: loop_status.break_point,
                }),
            );

            Ok(unused_block)
        }
        ast::Statement::LoopContinue => {
            let loop_status =
                current_loop.ok_or_else(|| StE::new(StatementError::UnwantedBreak))?;
            // make another block so everything continues normally, although it's going to
            // get removed afterwards.
            // TODO: make this return control flow so that I can make it stop processing when it
            // finds a break/continue (and forward the breaking if we aren't hitting any
            // branches/loop)
            let unused_block = state.new_block();

            builder.finish_block(
                state,
                BlockEnd::Branch(Branch::Unconditional {
                    target: loop_status.continue_point,
                }),
            );

            Ok(unused_block)
        }
        ast::Statement::Return((expr, expr_span)) => {
            let ret_value = bindings.next_binding();
            {
                let (mut block, resulting_value) = expr::compile_expr(
                    state,
                    builder,
                    expr,
                    bindings,
                    variables,
                    functions,
                    block_depth,
                    source_meta,
                )
                .map_err(|e| e.map_kind(From::from))?;
                block.assign(ret_value, resulting_value);
                Ok(block)
            }
            .map_err(|e: VarE| {
                e.with_backup_source(expr_span, source_meta)
                    .map_kind(From::from)
            })?
            .finish_block(state, ret_value);
            Ok(state.new_block())
        }
        ast::Statement::SingleExpr((expr, expr_span)) => {
            // create a dummy target that may or may not be cleaned up later,
            // depending on what it does
            let (mut block, result_expr) = expr::compile_expr(
                state,
                builder,
                expr,
                bindings,
                variables,
                functions,
                block_depth,
                source_meta,
            )
            .map_err(|e| {
                e.with_backup_source(expr_span, source_meta)
                    .map_kind(From::from)
            })?;
            let dummy = bindings.next_binding();
            block.assign(dummy, result_expr);
            Ok(block)
        }
        ast::Statement::DeclareVar {
            name: Source { source: name, span },
            init,
        } => {
            let memory = bindings.next_binding();
            builder.allocate(memory, 4); // all variables are 4-byte right now
                                         // compile init
            let builder = if let Some((init, init_span)) = init {
                let (mut builder, expr) = expr::compile_expr(
                    state,
                    builder,
                    init,
                    bindings,
                    variables,
                    functions,
                    block_depth,
                    source_meta,
                )
                .map_err(|e| {
                    e.with_backup_source(init_span, source_meta)
                        .map_kind(From::from)
                })?;
                let compute_init = bindings.next_binding();
                builder.assign(compute_init, expr);
                builder.store(compute_init, memory, ByteSize::U32);
                builder
            } else {
                builder
            };
            // add the variable to the index
            {
                let ctx = variables.variables_at_depth(block_depth);
                if ctx.contains_key(name) {
                    return Err(StE::new(VarError::Redeclared(name.to_string()).into())
                        .with_source(span, source_meta));
                } else {
                    ctx.insert(name, (memory, ByteSize::U32));
                }
            }
            Ok(builder)
        }
        ast::Statement::Block(block) => block::compile_block(
            state,
            builder,
            block,
            bindings,
            variables,
            functions,
            current_loop,
            block_depth + 1,
            source_meta,
        ),
        ast::Statement::IfStatement {
            condition: (condition_expr, condition_span),
            true_branch: (true_stmt, true_span),
            false_branch,
        } => {
            let (compute_condition, cond_flag) = {
                let (mut compute, cond_value) = expr::compile_expr(
                    state,
                    builder, // continue the builder we had previously
                    condition_expr,
                    bindings,
                    variables,
                    functions,
                    block_depth,
                    source_meta,
                )
                .map_err(|e| {
                    e.with_backup_source(condition_span, source_meta)
                        .map_kind(From::from)
                })?;
                let value_binding = bindings.next_binding();
                compute.assign(value_binding, cond_value);
                (compute, value_binding)
            };

            let (compute_if_true, true_head) = {
                let block = state.new_block();
                let head = block.block();
                (
                    compile_statement(
                        state,
                        block,
                        bindings,
                        *true_stmt,
                        variables,
                        functions,
                        current_loop,
                        block_depth,
                        source_meta,
                    )
                    .map_err(|e| e.with_backup_source(true_span, source_meta))?,
                    head,
                )
            };

            let (compute_if_false, false_head) = {
                let block = state.new_block();
                let head = block.block();
                (
                    if let Some((false_stmt, false_span)) = false_branch {
                        compile_statement(
                            state,
                            block,
                            bindings,
                            *false_stmt,
                            variables,
                            functions,
                            current_loop,
                            block_depth,
                            source_meta,
                        )
                        .map_err(|e| e.with_backup_source(false_span, source_meta))?
                    } else {
                        block
                    },
                    head,
                )
            };

            Ok(merge_branches(
                state,
                compute_condition,
                cond_flag,
                compute_if_true,
                true_head,
                compute_if_false,
                false_head,
            ))
        }
    }
}

/// Wire up branches so that they are merged in
pub fn merge_branches(
    state: &mut IRGenState,
    compute_condition: BlockBuilder,
    branch_flag: Binding,
    true_branch: BlockBuilder,
    true_branch_head: BlockBinding,
    false_branch: BlockBuilder,
    false_branch_head: BlockBinding,
) -> BlockBuilder {
    let mut end_block = state.new_block();
    // wire up the stuff so that the correct diamond graph is generated
    compute_condition.finish_block(
        state,
        Branch::Conditional {
            flag: branch_flag,
            target_true: true_branch_head,
            target_false: false_branch_head,
        },
    );

    true_branch.finish_block(
        state,
        Branch::Unconditional {
            target: end_block.block(),
        },
    );
    false_branch.finish_block(
        state,
        Branch::Unconditional {
            target: end_block.block(),
        },
    );

    end_block
}
