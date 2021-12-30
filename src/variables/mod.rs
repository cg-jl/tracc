use super::ast::Expr;
use super::ast::Statement;
use crate::ast::BinaryOp;
use crate::ast::Function;
use crate::ast::Identifier;
use crate::ast::Program;
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

// NOTE:maybe I should enable multiple-point errors, but that will have to wait for a better error
// crate
pub fn walk_program<'source>(
    Program(function): Program<'source>,
    source_meta: &SourceMetadata,
) -> Result<codegen::hlir::Program<'source>, error::Error<VarError>> {
    let function = walk_function(function, source_meta)?;
    Ok(codegen::hlir::Program(function))
}

fn walk_function<'source>(
    function: Function<'source>,
    source_meta: &SourceMetadata,
) -> Result<codegen::hlir::Function<'source>, error::Error<VarError>> {
    let Function {
        name: Identifier(name),
        body,
    } = function;
    let (body, var_amt) = walk_block(body.statements, source_meta)?;
    Ok(codegen::hlir::Function {
        body,
        name,
        var_amt,
    })
}

// core function that actually does the work for a block
fn walk_block<'source>(
    block: Vec<(Statement<'source>, Span)>,
    source_meta: &SourceMetadata,
) -> Result<(Vec<codegen::hlir::Statement>, usize), error::Error<VarError>> {
    // #1. Build the variable set. NOTE: we  don't want a variable to be visible before it's
    // declared, that's why I'm doing everything in the same loop.
    let mut var_set = HashMap::new();

    for (statement, _statement_span) in &block {
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

    // #3. Map those indices to the new variables and create the new statements
    Ok((
        block
            .into_iter()
            .filter_map(|(stmt, _)| match stmt {
                Statement::Return((expr, _)) => Some(codegen::hlir::Statement::Return(
                    convert_expr(expr, &indices),
                )),
                Statement::SingleExpr((expr, _)) => Some(codegen::hlir::Statement::Single(
                    convert_expr(expr, &indices),
                )),
                Statement::DeclareVar { name, init } => {
                    // if there is no init expression, just don't do nothing, the declaration
                    let init = init.map(|e| convert_expr(e.0, &indices))?;
                    // if the variable is used, generate the code to initialise it
                    Some(if let Some(&index) = indices.get(name.source) {
                        codegen::hlir::Statement::Single(codegen::hlir::Expr::Binary {
                            operator: BinaryOp::Assignment { op: None },
                            lhs: Box::new(codegen::hlir::Expr::Variable { index }),
                            rhs: Box::new(init),
                        })
                    } else {
                        codegen::hlir::Statement::Single(init)
                    })
                }
            })
            .collect::<Vec<_>>(),
        indices.len(),
    ))
}

// NOTE: the variables might all be indexable at this point and we can use the unwrapping []
fn convert_expr<'source>(
    expr: Expr<'source>,
    indices: &HashMap<&'source str, usize>,
) -> codegen::hlir::Expr {
    match expr {
        // all variables that are passed here already exist
        Expr::Variable { name } => {
            let index = indices[name.source];
            codegen::hlir::Expr::Variable { index }
        }
        Expr::Constant(c) => codegen::hlir::Expr::Constant(c),
        Expr::Unary { operator, expr } => {
            let inner = convert_expr(*expr.0, indices);
            codegen::hlir::Expr::Unary {
                operator,
                inner: Box::new(inner),
            }
        }
        Expr::Binary { operator, lhs, rhs } => {
            let lhs = convert_expr(*lhs.0, indices);
            let rhs = convert_expr(*rhs.0, indices);
            codegen::hlir::Expr::Binary {
                operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }
    }
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
            Expr::Constant(_) => (),
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
