use crate::ast::{BinaryOp, Block, Expr, Function, Identifier, Program, Statement};
use crate::codegen;
use crate::error::SourceMetadata;
use crate::error::{self, Span};
use crate::grammar::lexer::Source;
use std::collections::HashMap;
use std::fmt;

// the purpose of this is to walk the entire block looking for variables,
// and check all the expressions in case they have bad refs.
// furthermore, it's going to remove all the variables which are unused.

// the error is put through the pipeline before the data goes into the codegen unit.
// now, the codegen unit has to know the number of variables in advance.

// NOTE: all variables are currently integers. no type info is taken into account.

// NOTE: this is a result of the bad error formatting library I'm using.
#[derive(Debug)]
pub enum VarError {
    Undeclared(String),
    Redeclared(String),
}
impl fmt::Display for VarError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undeclared(name) => write!(f, "unknown variable: `{}`", name),
            Self::Redeclared(name) => {
                write!(f, "variable `{}` was already declared in this scope", name)
            }
        }
    }
}

impl std::error::Error for VarError {}

// fn walk_block<'code>(block: Vec<(Statement<'code>, Span)>, )

type UsedMap<'code> = HashMap<&'code str, bool>;
type IndexMap<'code> = HashMap<&'code str, usize>;

pub fn convert_program<'code>(
    program: Program<'code>,
    source_meta: &SourceMetadata,
) -> Result<codegen::hlir::Program<'code>, error::Error<VarError>> {
    let Program(function) = program;
    convert_function(function, source_meta).map(codegen::hlir::Program)
}

fn convert_function<'code>(
    func: Function<'code>,
    source_meta: &SourceMetadata,
) -> Result<codegen::hlir::Function<'code>, error::Error<VarError>> {
    let Function {
        name: Identifier(name),
        body: Block { statements: block },
    } = func;

    let dummy_index_map = IndexMap::new();
    let (body, var_amt) = convert_block(block, 0, &dummy_index_map, source_meta)?;

    Ok(codegen::hlir::Function {
        name,
        body,
        var_amt,
    })
}

fn convert_block<'code>(
    block: Vec<(Statement<'code>, Span)>,
    variables_offset: usize,
    index_map: &IndexMap<'code>,
    source_meta: &SourceMetadata,
) -> Result<(Vec<codegen::hlir::Statement>, usize), error::Error<VarError>> {
    let mut used_map = UsedMap::new();
    let mut total_statements = 0usize;

    // pass #1: Make a pass to know which variables are being used. This ignores unknown variables
    for (statement, statement_span) in &block {
        total_statements += match statement {
            // declarations when converting the block are treated differently as
            // we need to detect redeclarations
            Statement::DeclareVar {
                name: Source { source: name, .. },
                init,
            } => {
                // redeclarations here are penalized
                if used_map.contains_key(name) {
                    return Err(error::Error::new(VarError::Redeclared(name.to_string()))
                        .with_source(*statement_span, source_meta));
                }
                if let Some((init, _)) = init {
                    walk_expr_used_vars(init, &mut used_map);
                }
                used_map.insert(name, false);
                0usize
            }
            statement => walk_statement_used_vars(statement, &mut used_map),
        };
    }

    // now create indices for all those variables that were used
    let index_map: IndexMap = index_map
        .clone()
        .into_iter()
        .chain(
            used_map
                .into_iter()
                .filter(|(_, is_used)| *is_used)
                .enumerate()
                .map(|(index, (name, _))| (name, index + variables_offset)),
        )
        .collect();

    // pass #2: Convert all the statements to HLIR. This pass does error for unknown/redeclared variables

    // full block length is the max length it could have. It can have *less*, but not more than those
    let mut final_hlir_code = Vec::with_capacity(total_statements);
    let mut max_extra_amount = 0usize;

    for (statement, _) in block {
        // NOTE: these potential errors don't need a backup source because all of the paths of
        // `stmt_assign_indices` assign a source
        let (hlir, extra_variables) = stmt_assign_indices(
            statement,
            index_map.len() + variables_offset,
            &index_map,
            source_meta,
        )?;
        final_hlir_code.extend(hlir);
        max_extra_amount = max_extra_amount.max(extra_variables);
    }

    Ok((final_hlir_code, index_map.len() + max_extra_amount))
}

// ------------
// walks for used variables. Note that any of these don't add any variables

// gets the max number of statements per statement kind as well
fn walk_statement_used_vars<'code>(
    statement: &Statement<'code>,
    used_map: &mut UsedMap<'code>,
) -> usize {
    match statement {
        Statement::Return((expr, _)) => {
            walk_expr_used_vars(expr, used_map);
            1
        }
        Statement::SingleExpr((expr, _)) => {
            walk_expr_used_vars(expr, used_map);
            1
        }
        Statement::DeclareVar { name: _, init } => {
            if let Some((init, _)) = init {
                walk_expr_used_vars(init, used_map);
            }
            1
        }
        Statement::Block(block) => {
            walk_block_used_vars(block, used_map);
            block.len()
        }
        Statement::IfStatement {
            condition: (condition, _),
            true_branch: (true_branch, _),
            false_branch,
        } => {
            walk_expr_used_vars(condition, used_map);
            let true_branch_stmts = walk_statement_used_vars(true_branch, used_map);
            if let Some((false_branch, _)) = false_branch {
                true_branch_stmts.max(walk_statement_used_vars(false_branch, used_map))
            } else {
                true_branch_stmts
            }
        }
    }
}

fn walk_block_used_vars<'code>(block: &[(Statement<'code>, Span)], used_map: &mut UsedMap<'code>) {
    for (statement, _) in block {
        walk_statement_used_vars(statement, used_map);
    }
}

fn walk_expr_used_vars<'code>(expr: &Expr<'code>, used_map: &mut UsedMap<'code>) {
    match expr {
        Expr::Variable {
            name: Source { source: name, .. },
        } => {
            if let Some(is_used) = used_map.get_mut(name) {
                *is_used = true;
            }
        }
        Expr::Constant(_) => (),
        Expr::Unary {
            operator: _,
            expr: (expr, _),
        } => walk_expr_used_vars(expr, used_map),
        Expr::Binary {
            operator: _,
            lhs: (lhs, _),
            rhs: (rhs, _),
        } => {
            walk_expr_used_vars(lhs, used_map);
            walk_expr_used_vars(rhs, used_map);
        }
    }
}

// ----- assignment routines ----

fn stmt_assign_indices<'code>(
    statement: Statement<'code>,
    variables_offset: usize,
    index_map: &IndexMap<'code>,
    source_meta: &SourceMetadata,
) -> Result<(Vec<codegen::hlir::Statement>, usize), error::Error<VarError>> {
    match statement {
        Statement::Return((expr, expr_span)) => {
            let expr = expr_assign_indices(expr, index_map, source_meta)
                .map_err(|err| err.with_backup_source(expr_span, source_meta))?;
            // no variables were added
            Ok((vec![codegen::hlir::Statement::Return(expr)], 0))
        }
        Statement::SingleExpr((expr, expr_span)) => {
            let expr = expr_assign_indices(expr, index_map, source_meta)
                .map_err(|err| err.with_backup_source(expr_span, source_meta))?;
            // no variables were added
            Ok((vec![codegen::hlir::Statement::Single(expr)], 0))
        }
        Statement::DeclareVar {
            name: Source { source: name, .. },
            init,
        } => {
            // only do something if an init expression is given, otherwise
            // it was just an annotation for the variable processor
            let result_vec = if let Some((init_expr, init_span)) = init {
                let init = expr_assign_indices(init_expr, index_map, source_meta)
                    .map_err(|err| err.with_backup_source(init_span, source_meta))?;
                vec![codegen::hlir::Statement::Single(
                    // if the variable was not discarded, then give an assignment expression,
                    // otherwise just give it the init expression
                    if let Some(&index) = index_map.get(name) {
                        codegen::hlir::Expr::Binary {
                            operator: BinaryOp::Assignment { op: None },
                            lhs: Box::new(codegen::hlir::Expr::Variable { index }),
                            rhs: Box::new(init),
                        }
                    } else {
                        init
                    },
                )]
            } else {
                Vec::new()
            };
            Ok((result_vec, 0))
        }
        Statement::Block(block) => convert_block(block, variables_offset, index_map, source_meta),
        Statement::IfStatement {
            condition: (condition, condition_span),
            true_branch: (true_branch, true_branch_span),
            false_branch,
        } => {
            let condition = expr_assign_indices(condition, index_map, source_meta)
                .map_err(|err| err.with_backup_source(condition_span, source_meta))?;
            let (true_branch, true_extra_variables) =
                stmt_assign_indices(*true_branch, variables_offset, index_map, source_meta)
                    .map_err(|err| err.with_backup_source(true_branch_span, source_meta))?;
            let (false_branch, false_extra_variables) = if let Some((branch, extra_vars)) =
                false_branch
                    .map(|(stmt, span)| {
                        stmt_assign_indices(*stmt, variables_offset, index_map, source_meta)
                            .map_err(|err| err.with_backup_source(span, source_meta))
                    })
                    .transpose()?
            {
                (Some(branch), extra_vars)
            } else {
                (None, 0)
            };

            Ok((
                vec![codegen::hlir::Statement::IfStatement {
                    condition,
                    true_branch,
                    false_branch,
                }],
                true_extra_variables.max(false_extra_variables),
            ))
        }
    }
}

fn expr_assign_indices<'code>(
    expr: Expr<'code>,
    index_map: &IndexMap<'code>,
    source_meta: &SourceMetadata,
) -> Result<codegen::hlir::Expr, error::Error<VarError>> {
    match expr {
        Expr::Variable {
            name: Source { source: name, .. },
        } => {
            let index = index_map
                .get(name)
                .copied()
                .ok_or_else(|| error::Error::new(VarError::Undeclared(name.to_string())))?;
            Ok(codegen::hlir::Expr::Variable { index })
        }
        Expr::Constant(c) => Ok(codegen::hlir::Expr::Constant(c)),
        Expr::Unary {
            operator,
            expr: (expr, inner_span),
        } => {
            let inner = expr_assign_indices(*expr, index_map, source_meta)
                .map_err(|err| err.with_backup_source(inner_span, source_meta))?;
            Ok(codegen::hlir::Expr::Unary {
                operator,
                inner: Box::new(inner),
            })
        }
        Expr::Binary {
            operator,
            lhs: (lhs, lhs_span),
            rhs: (rhs, rhs_span),
        } => {
            let lhs = expr_assign_indices(*lhs, index_map, source_meta)
                .map_err(|err| err.with_backup_source(lhs_span, source_meta))?;
            let rhs = expr_assign_indices(*rhs, index_map, source_meta)
                .map_err(|err| err.with_backup_source(rhs_span, source_meta))?;
            Ok(codegen::hlir::Expr::Binary {
                operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }
    }
}
