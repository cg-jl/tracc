use super::ast::Block;
use super::ast::Expr;
use super::ast::Statement;
use crate::ast::Function;
use crate::ast::Program;
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

pub fn walk_program<'source>(
    mut program: Program<'source>,
    source_meta: &SourceMetadata,
) -> Result<Program<'source>, error::Error<VarError>> {
    walk_function(&mut program.0, source_meta)?;
    Ok(program)
}

fn walk_function(
    function: &mut Function,
    source_meta: &SourceMetadata,
) -> Result<(), error::Error<VarError>> {
    walk_block(&mut function.body, source_meta)
}

// core function that actually does the work for a block
fn walk_block(
    block: &mut Block,
    source_meta: &SourceMetadata,
) -> Result<(), error::Error<VarError>> {
    // #1. Build the variable set. NOTE: we  don't want a variable to be visible before it's
    // declared, that's why I'm doing everything in the same loop.
    let mut var_set = HashMap::new();

    for (statement, _statement_span) in &block.statements {
        match statement {
            Statement::DeclareVar { name, init } => {
                let Source { span, source: name } = name;
                // walk the init expression first to avoid declarations like `int a = a;`
                if let Some((init_expr, init_span)) = init {
                    walk_expr(init_expr, *init_span, &mut var_set, source_meta)?;
                }
                if var_set.contains_key(*name) {
                    return Err(error::Error::new(VarError::Redeclared(name.to_string()))
                        .with_source(*span, source_meta));
                } else {
                    var_set.insert(*name, false);
                }
            }
            Statement::Return((expr, expr_span)) => {
                walk_expr(expr, *expr_span, &mut var_set, source_meta)?
            }
            Statement::SingleExpr((expr, expr_span)) => {
                walk_expr(expr, *expr_span, &mut var_set, source_meta)?
            }
        }
    }

    // #2. Create indices for the referenced variables
    let indices: HashMap<_, _> = var_set
        .into_iter()
        .filter(|(_, is_used)| *is_used)
        .map(|x| x.0)
        .enumerate()
        .map(|(a, b)| (b, a))
        .collect();

    block.variables.write(indices);
    Ok(())
}

fn walk_expr<'source>(
    expr: &Expr<'source>,
    expr_span: Span,
    ctx: &mut HashMap<&'source str, bool>,
    source_meta: &SourceMetadata,
) -> Result<(), error::Error<VarError>> {
    let mut queue = vec![(expr, expr_span)];

    while let Some((expr, expr_span)) = queue.pop() {
        match expr {
            Expr::Variable { name } => {
                let Source {
                    span: _,
                    source: name,
                } = name;
                *ctx.get_mut(*name).ok_or_else(|| {
                    error::Error::new(VarError::Undeclared(name.to_string()))
                        .with_source(expr_span, source_meta)
                })? = true;
            }
            Expr::Unary { expr, .. } => queue.push((&expr.0, expr.1)),
            Expr::Binary { lhs, rhs, .. } => {
                // binary expression: just visit both subtrees.
                queue.push((&lhs.0, lhs.1));
                queue.push((&rhs.0, rhs.1));
            }
            Expr::Constant(_) | Expr::AlreadyInTarget => (),
        }
    }

    Ok(())
}

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
