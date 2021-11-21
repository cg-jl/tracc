use super::ast::Block;
use super::ast::Expr;
use super::ast::Statement;
use super::ast::VariableKind;
use crate::ast::BinaryOp;
use std::collections::HashMap;

// NOTE: all variables are currently integers. no type info is taken into account.
// HACK: this is just a bootstrapper for variables

pub fn walk_block(Block(statements): &mut Block) -> usize {
    let mut ctx = HashMap::new();
    for st in statements.iter_mut() {
        walk_statement(st, &mut ctx);
    }

    // remove all the declare var statements with no initializer
    statements.retain(|x| !matches!(x, Statement::DeclareVar { init: None, .. }));
    // transform all the declare var statements to assign operations
    for st in statements.iter_mut() {
        let new_statement = match std::mem::take(st) {
            Statement::DeclareVar {
                init: Some(mut expr),
                name,
            } => {
                walk_expr(&mut expr, &ctx);
                Statement::SingleExpr(Expr::Binary {
                    operator: BinaryOp::Assign,
                    // using ctx[] because it HAS to be there; we just process it.
                    lhs: Box::new(Expr::Variable(VariableKind::Processed {
                        index: ctx[&name],
                    })),
                    rhs: Box::new(expr),
                })
            }
            other => other,
        };
        *st = new_statement;
    }

    ctx.len()
}

fn walk_statement(statement: &mut Statement, context: &mut HashMap<String, usize>) {
    match statement {
        Statement::DeclareVar { name, .. } => {
            context.insert(name.to_string(), context.len());
        }
        Statement::Return(expr) => {
            walk_expr(expr, context);
        }
        Statement::SingleExpr(expr) => {
            walk_expr(expr, context);
        }
    }
}

fn walk_expr(expr: &mut Expr, context: &HashMap<String, usize>) {
    match expr {
        Expr::Binary { lhs, rhs, .. } => {
            walk_expr(lhs, context);
            walk_expr(rhs, context);
        }
        Expr::Constant(_) => (),
        Expr::Unary { expr, .. } => walk_expr(expr, context),
        Expr::Variable(kind) => {
            if let VariableKind::Unprocessed(name) = kind {
                // TODO: better errors at walking
                let index = context
                    .get(name)
                    .copied()
                    // TODO: this is a user error, and must be reported through other means.
                    .unwrap_or_else(|| panic!("no variable named `{}` in scope", name));
                *kind = VariableKind::Processed { index };
            } // otherwise, leave it untouched
        }
    }
}
