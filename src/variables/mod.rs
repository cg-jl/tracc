use super::ast::Block;
use super::ast::Expr;
use super::ast::Statement;
use super::ast::VariableKind;
use crate::ast::BinaryOp;
use std::collections::HashMap;

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
pub fn walk_block(Block(statements): Block) -> (Block, usize) {
    let mut ctx = HashMap::new();

    // 1. Walk the statements looking for variables
    for st in statements.iter() {
        match st {
            Statement::DeclareVar { name, .. } => {
                if ctx.get(name).is_some() {
                    panic!("variable `{}` is already declared in this scope", name);
                }
                ctx.insert(name.to_string(), 0);
            }
            Statement::Return(expr) => count_refs(expr, &mut ctx),
            Statement::SingleExpr(expr) => count_refs(expr, &mut ctx),
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
            .zip(1usize..)
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
                        rhs: Box::new(init),
                    })
                }),
                Statement::Return(expr) => Some(Statement::Return(assign_indices(expr, &vars))),
                Statement::SingleExpr(expr) => {
                    Some(Statement::SingleExpr(assign_indices(expr, &vars)))
                }
            })
            .collect(),
    );

    (block, vars.len())
}

fn assign_indices(expr: Expr, vars: &HashMap<String, usize>) -> Expr {
    match expr {
        Expr::Binary { lhs, rhs, operator } => Expr::Binary {
            operator,
            lhs: Box::new(*lhs),
            rhs: Box::new(*rhs),
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

fn count_refs(expr: &Expr, ctx: &mut HashMap<String, usize>) {
    match expr {
        Expr::Binary { lhs, rhs, .. } => {
            count_refs(lhs, ctx);
            count_refs(rhs, ctx);
        }
        Expr::Variable(VariableKind::Unprocessed(name)) => {
            *ctx.get_mut(name)
                .unwrap_or_else(|| panic!("undeclared variable: `{}`", name)) += 1;
        }
        Expr::Variable(VariableKind::Processed { .. }) => {
            unreachable!("shouldn't have processed variables yet")
        }
        Expr::Unary { expr, .. } => count_refs(expr, ctx),
        _ => (),
    }
}

// pub fn walk_block(Block(statements): &mut Block) -> usize {
//     let mut ctx = HashMap::new();
//     for st in statements.iter_mut() {
//         walk_statement(st, &mut ctx);
//     }

//     // remove all the declare var statements with no initializer
//     statements.retain(|x| !matches!(x, Statement::DeclareVar { init: None, .. }));
//     // transform all the declare var statements to assign operations
//     for st in statements.iter_mut() {
//         let new_statement = match std::mem::take(st) {
//             Statement::DeclareVar {
//                 init: Some(mut expr),
//                 name,
//             } => {
//                 walk_expr(&mut expr, &ctx);
//                 Statement::SingleExpr(Expr::Binary {
//                     operator: BinaryOp::Assign,
//                     // using ctx[] because it HAS to be there; we just process it.
//                     lhs: Box::new(Expr::Variable(VariableKind::Processed {
//                         index: ctx[&name],
//                     })),
//                     rhs: Box::new(expr),
//                 })
//             }
//             other => other,
//         };
//         *st = new_statement;
//     }

//     ctx.len()
// }

// fn walk_statement(statement: &mut Statement, context: &mut HashMap<String, usize>) {
//     match statement {
//         Statement::DeclareVar { name, .. } => {
//             context.insert(name.to_string(), context.len());
//         }
//         Statement::Return(expr) => {
//             walk_expr(expr, context);
//         }
//         Statement::SingleExpr(expr) => {
//             walk_expr(expr, context);
//         }
//     }
// }

// fn walk_expr(expr: &mut Expr, context: &HashMap<String, usize>) {
//     match expr {
//         Expr::Binary { lhs, rhs, .. } => {
//             walk_expr(lhs, context);
//             walk_expr(rhs, context);
//         }
//         Expr::Constant(_) => (),
//         Expr::Unary { expr, .. } => walk_expr(expr, context),
//         Expr::Variable(kind) => {
//             if let VariableKind::Unprocessed(name) = kind {
//                 // TODO: better errors at walking
//                 let index = context
//                     .get(name)
//                     .copied()
//                     // TODO: this is a user error, and must be reported through other means.
//                     .unwrap_or_else(|| panic!("no variable named `{}` in scope", name));
//                 *kind = VariableKind::Processed { index };
//             } // otherwise, leave it untouched
//         }
//     }
// }
