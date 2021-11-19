use super::ast::Block;
use super::ast::Expr;
use super::ast::Statement;
use super::ast::VariableKind;
use std::collections::HashMap;

// NOTE: all variables are currently integers. no type info is taken into account.
// HACK: this is just a bootstrapper for variables

pub fn walk_block(Block(statements): &mut Block) -> usize {
    let mut ctx = HashMap::new();
    for (i, st) in statements.iter_mut().enumerate() {
        walk_statement(st, &mut ctx);
    }

    statements.retain(|x| !matches!(x, Statement::DeclareVar(_)));

    ctx.len()
}

fn walk_statement(statement: &mut Statement, context: &mut HashMap<String, usize>) {
    match statement {
        Statement::DeclareVar(name) => {
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

fn walk_expr(expr: &mut Expr, context: &mut HashMap<String, usize>) {
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
                    .map(|x| *x)
                    .expect(&format!("no variable named `{}` in scope", name));

                *kind = VariableKind::Processed { index };
            } // otherwise, leave it untouched
        }
    }
}
