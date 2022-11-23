use super::*;
use crate::{ast, error::Span};

pub fn compile_block<'code>(
    state: &mut IRGenState,
    mut builder: BlockBuilder,
    statements: impl IntoIterator<Item = (ast::Statement<'code>, Span)>,
    bindings: &mut BindingCounter,
    variables: &mut VariableTracker<'code>,
    functions: &HashMap<&'code str, BlockBinding>,
    current_loop: Option<statement::LoopStatus>,
    block_depth: usize,
    source_info: &SourceMetadata,
) -> Result<BlockBuilder, StE> {
    for (st, st_span) in statements {
        builder = statement::compile_statement(
            state,
            builder,
            bindings,
            st,
            variables,
            functions,
            current_loop,
            block_depth,
            source_info,
        )
        .map_err(|e| e.with_backup_source(st_span, source_info))?;
    }
    Ok(builder)
}
