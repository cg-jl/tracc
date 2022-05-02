use super::*;
use crate::ast;

pub fn compile_statement<'code>(
    state: &mut IRGenState,
    mut builder: BlockBuilder,
    bindings: &mut BindingCounter,
    statement: ast::Statement<'code>,
    variables: &mut VariableTracker<'code>,
    block_depth: usize,
    source_meta: &SourceMetadata,
) -> Result<BlockBuilder, VarE> {
    match statement {
        ast::Statement::LoopBreak | ast::Statement::Loop { .. } | ast::Statement::LoopContinue => {
            todo!("loops")
        }
        ast::Statement::Return((expr, expr_span)) => {
            let ret_value = bindings.next_binding();
            {
                let (mut block, resulting_value) =
                    expr::compile_expr(state, builder, expr, bindings, variables, source_meta)?;
                block.assign(ret_value, resulting_value);
                Ok(block)
            }
            .map_err(|e: VarE| e.with_backup_source(expr_span, source_meta))?
            .finish_block(state, ret_value);
            Ok(state.new_block())
        }
        ast::Statement::SingleExpr((expr, expr_span)) => {
            // create a dummy target that may or may not be cleaned up later,
            // depending on what it does
            let (mut block, result_expr) =
                expr::compile_expr(state, builder, expr, bindings, variables, source_meta)
                    .map_err(|e| e.with_backup_source(expr_span, source_meta))?;
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
                let (mut builder, expr) =
                    expr::compile_expr(state, builder, init, bindings, variables, source_meta)
                        .map_err(|e| e.with_backup_source(init_span, source_meta))?;
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
                    return Err(VarE::new(VarError::Redeclared(name.to_string()))
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
                    source_meta,
                )
                .map_err(|e| e.with_backup_source(condition_span, source_meta))?;
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
