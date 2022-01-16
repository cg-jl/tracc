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
        ast::Statement::Return((expr, expr_span)) => {
            let ret_value = bindings.next_binding();
            expr::compile_expr(
                state,
                builder,
                ret_value,
                expr,
                bindings,
                variables,
                source_meta,
            )
            .map_err(|e| e.with_backup_source(expr_span, source_meta))?
            .finish_block(state, ret_value);
            Ok(state.new_block())
        }
        ast::Statement::SingleExpr((expr, expr_span)) => {
            // create a dummy target that may or may not be cleaned up later,
            // depending on what it does
            let dummy = bindings.next_binding();
            expr::compile_expr(
                state,
                builder,
                dummy,
                expr,
                bindings,
                variables,
                source_meta,
            )
            .map_err(|e| e.with_backup_source(expr_span, source_meta))
        }
        ast::Statement::DeclareVar {
            name: Source { source: name, span },
            init,
        } => {
            let memory = bindings.next_binding();
            builder.allocate(memory, 4); // all variables are 4-byte right now
                                         // compile init
            let builder = if let Some((init, init_span)) = init {
                let compute_init = bindings.next_binding();
                let mut builder = expr::compile_expr(
                    state,
                    builder,
                    compute_init,
                    init,
                    bindings,
                    variables,
                    source_meta,
                )
                .map_err(|e| e.with_backup_source(init_span, source_meta))?;
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
            let condition = bindings.next_binding();
            let mut builder = expr::compile_expr(
                state,
                builder,
                condition,
                condition_expr,
                bindings,
                variables,
                source_meta,
            )
            .map_err(|e| e.with_backup_source(condition_span, source_meta))?;
            let flag = bindings.next_binding();
            builder.assign(
                flag,
                Value::Cmp {
                    condition: Condition::NotEquals,
                    lhs: condition,
                    rhs: 0.into(),
                },
            );
            let end_block = state.new_block();
            let true_block = {
                let block = state.new_block();
                let start = block.block();
                let true_block_end = compile_statement(
                    state,
                    block,
                    bindings,
                    *true_stmt,
                    variables,
                    block_depth,
                    source_meta,
                )
                .map_err(|e| e.with_backup_source(true_span, source_meta))?;
                true_block_end.finish_block(
                    state,
                    Branch::Unconditional {
                        target: end_block.block(),
                    },
                );
                start
            };
            let false_block = if let Some((false_stmt, false_span)) = false_branch {
                let block = state.new_block();
                let start = block.block();
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
                .finish_block(
                    state,
                    Branch::Unconditional {
                        target: end_block.block(),
                    },
                );
                start // give the start of the block to branch
            } else {
                end_block.block()
            };

            builder.finish_block(
                state,
                Branch::Conditional {
                    flag,
                    target_true: true_block,
                    target_false: false_block,
                },
            );
            Ok(end_block)
        }
    }
}
