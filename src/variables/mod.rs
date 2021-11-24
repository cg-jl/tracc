use super::ast::Block;
use super::ast::Expr;
use super::ast::Statement;
use super::ast::VariableKind;
use crate::ast::BinaryOp;
use std::collections::HashMap;
use std::fmt;

// struct VarCtx {
//     inner: HashMap<String, (usize, usize)>,
// }

// impl VarCtx {
//     pub fn new() -> Self {
//         Self {
//             inner: HashMap::new(),
//         }
//     }
//     pub fn declare_variable(&mut self, name: String) {
//         self.inner.insert(name, (self.inner.len(), 0));
//     }
//     pub fn get_variable(&mut self, name: &str) -> Option<usize> {
//         self.inner.get(name).map(|x| x.0)
//     }
// }

// NOTE: all variables are currently integers. no type info is taken into account.
// HACK: this is just a bootstrapper for variables
pub fn walk_block(Block(statements): Block) -> Result<(Block, usize), VarError> {
    let mut ctx = HashMap::new();

    // 1. Walk the statements looking for variables
    for st in statements.iter() {
        match st {
            Statement::DeclareVar { name, .. } => {
                if ctx.get(name).is_some() {
                    return Err(VarError::Redeclared(name.to_string()));
                }
                ctx.insert(name.to_string(), 0);
            }
            Statement::Return(expr) => count_refs(expr, &mut ctx)?,
            Statement::SingleExpr(expr) => count_refs(expr, &mut ctx)?,
        }
    }

    // 2. clean all the unused variables
    ctx.retain(|name, ref_count| {
        if *ref_count == 0 {
            println!("warning: unused variable: `{}`", name);
            false
        } else {
            true
        }
    });

    // 3. Assign indices to the used variables
    let vars: HashMap<_, _> = {
        let mut ctx_vec: Vec<_> = ctx.into_iter().collect();
        ctx_vec.sort_by_key(|(_, ref_count)| *ref_count);
        ctx_vec
            .into_iter()
            .map(|(name, _)| name)
            .zip(0usize..)
            .collect()
    };

    // 4. Update the block
    let block = Block(
        statements
            .into_iter()
            .filter_map(|st| match st {
                Statement::DeclareVar { init, name } => init.map(|init| {
                    Statement::SingleExpr(Expr::Binary {
                        operator: BinaryOp::Assign,
                        lhs: Box::new(Expr::Variable(VariableKind::Processed {
                            index: vars[&name],
                        })),
                        rhs: Box::new(assign_indices(init, &vars)),
                    })
                }),
                Statement::Return(expr) => Some(Statement::Return(assign_indices(expr, &vars))),
                Statement::SingleExpr(expr) => {
                    Some(Statement::SingleExpr(assign_indices(expr, &vars)))
                }
            })
            .collect(),
    );

    Ok((block, vars.len()))
}

fn assign_indices(expr: Expr, vars: &HashMap<String, usize>) -> Expr {
    match expr {
        Expr::Binary { lhs, rhs, operator } => Expr::Binary {
            operator,
            lhs: Box::new(assign_indices(*lhs, vars)),
            rhs: Box::new(assign_indices(*rhs, vars)),
        },
        Expr::Variable(kind) => Expr::Variable(match kind {
            k @ VariableKind::Processed { .. } => k,
            VariableKind::Unprocessed(name) => VariableKind::Processed { index: vars[&name] },
        }),
        k @ Expr::Constant(_) => k,
        Expr::Unary { operator, expr } => Expr::Unary {
            operator,
            expr: Box::new(assign_indices(*expr, vars)),
        },
    }
}

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

fn count_refs(expr: &Expr, ctx: &mut HashMap<String, usize>) -> Result<(), VarError> {
    match expr {
        Expr::Binary { lhs, rhs, .. } => {
            count_refs(lhs, ctx)?;
            count_refs(rhs, ctx)?;
        }
        Expr::Variable(VariableKind::Unprocessed(name)) => {
            *ctx.get_mut(name)
                .ok_or_else(|| VarError::Undeclared(name.to_string()))? += 1;
        }
        Expr::Variable(VariableKind::Processed { .. }) => {
            unreachable!("shouldn't have processed variables yet")
        }
        Expr::Unary { expr, .. } => count_refs(expr, ctx)?,
        _ => (),
    };
    Ok(())
}
