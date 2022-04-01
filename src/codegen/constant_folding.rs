use crate::ast::{ArithmeticOp, BinaryOp, BitOp, LogicOp, OpFlags, UnaryOp};

use super::hlir::{Expr, Statement};

pub fn reorder_and_fold_block(
    block: impl IntoIterator<Item = Statement>,
) -> impl Iterator<Item = Statement> {
    block.into_iter().map(reorder_and_fold_statement)
}

// right now only does stuff with the expressions
// XXX: I have to do this recursive because I don't have yet a nice linear IR
pub fn reorder_and_fold_statement(stmt: Statement) -> Statement {
    match stmt {
        Statement::Loop {
            condition,
            block,
            condition_at_end,
        } => Statement::Loop {
            condition: reduce_and_reorder_expr(condition),
            block: reorder_and_fold_block(block).collect(),
            condition_at_end,
        },
        Statement::Return(ret_expr) => Statement::Return(reduce_and_reorder_expr(ret_expr)),
        Statement::Single(expr) => Statement::Single(reduce_and_reorder_expr(expr)),
        Statement::IfStatement {
            condition,
            true_branch,
            false_branch,
        } => Statement::IfStatement {
            condition: reduce_and_reorder_expr(condition),
            true_branch: reorder_and_fold_block(true_branch).collect(),
            false_branch: false_branch.map(|b| reorder_and_fold_block(b).collect()),
        },
        // these can't be reordered, they're special jumps.
        Statement::LoopBreak => Statement::LoopBreak,
        Statement::LoopContinue => Statement::LoopContinue,
    }
}

/// Reorder binary expressions using commutative/associative laws so that all the constants end up
/// together (with maybe other binary expressions) and on the right side
fn reorder_binary_expr(
    op: impl OpFlags + PartialEq<BinaryOp> + Copy,
    lhs: Expr,
    rhs: Expr,
) -> (Expr, Expr) {
    match (reduce_expr(lhs), reduce_expr(rhs)) {
        // we're looking at a case like (a o c) o (b o d).
        // We can do the following optimizations:
        (
            Expr::Binary {
                operator: op1,
                lhs: a,
                rhs: b,
                branch_depends_on_result: _,
            },
            Expr::Binary {
                operator: op2,
                lhs: c,
                rhs: d,
                branch_depends_on_result: _,
            },
        ) if op == op1 && op == op2 => {
            let (a, b) = reorder_binary_expr(op, *a, *b);
            let (c, d) = reorder_binary_expr(op, *c, *d);
            match (a, b, c, d) {
                //  - if the expression looks like (a o k1) o (b o k2) and the operator is both commutative
                //    and associative, we can do the following manipulation:
                //      (a o k1) o (b o k2)
                //    = a o (k1 o (b o k2)) ; associativity
                //    = a o (k1 o (k2 o b)) ; commutativity
                //    = a o ((k1 o k2) o b) ; associativity
                //    = a o (b o (k1 o k2)) ; commutativity
                //    = (a o b) o (k1 o k2) ; associativity
                //    and reduce the constant to have a K = k1 o k2
                //    = (a o b) o K
                (a, k1 @ Expr::Constant(_), b, k2 @ Expr::Constant(_))
                    if op.is_associative() && op.is_commutative() =>
                {
                    (
                        Expr::Binary {
                            operator: op1,
                            lhs: Box::new(a),
                            rhs: Box::new(b),
                            branch_depends_on_result: false,
                        },
                        reduce_binary_expr(op1, k1, k2),
                    )
                }
                // - if the expression looks like (a o k1) o (k2 o b) and the operator is
                // associative, we can get it to look like:
                //  (a o k1) o (k2 o b)
                //= a o ((k1 o k2) o b) ; associativity
                // if we name k = k1 o k2, we have
                //= a o (k o b), which is already better than the last one (one less operation,
                // same number of registers)
                // if the operator is commutative too, we can reduce the register count by douing
                // the following operations:
                //  a o (k o b)
                //= a o (b o k) ; commutativity
                //= (a o b) o k ; associativity
                //and we can have 2 operations and 2  registers (instead of 3)
                (a, k1 @ Expr::Constant(_), k2 @ Expr::Constant(_), b) => {
                    let k = reduce_binary_expr(op1, k1, k2);
                    if op.is_commutative() {
                        // (a o b) o k
                        (
                            Expr::Binary {
                                operator: op1,
                                lhs: Box::new(a),
                                rhs: Box::new(b),
                                branch_depends_on_result: false,
                            },
                            k,
                        )
                    } else {
                        // a o (k o b)
                        (
                            a,
                            Expr::Binary {
                                operator: op1,
                                lhs: Box::new(k),
                                rhs: Box::new(b),
                                branch_depends_on_result: false,
                            },
                        )
                    }
                }
                // there are no other ways to improve the code that I can see with this pattern
                (a, b, c, d) => (
                    Expr::Binary {
                        operator: op1,
                        lhs: Box::new(a),
                        rhs: Box::new(b),
                        branch_depends_on_result: false,
                    },
                    Expr::Binary {
                        operator: op1,
                        lhs: Box::new(c),
                        rhs: Box::new(d),
                        branch_depends_on_result: false,
                    },
                ),
            }
        }
        // we are looking at an expression in the form (a o b) o c
        (
            Expr::Binary {
                operator: op1,
                lhs: a,
                rhs: b,
                branch_depends_on_result: false,
            },
            c,
        ) if op == op1 => {
            let (a, b) = reorder_binary_expr(op, *a, *b);
            match (a, b, c) {
                // if the expression looks like (k1 o a) o k2, we can do the following:
                //  (k1 o a) o k2
                //= k1 o (a o k2) ; associativity
                //= k1 o (k2 o a) ; commutativity
                //= (k1 o k2) o a ; associativity
                //(let k = k1 o k2)
                //= k o a = a o k ; commutativity
                (k1 @ Expr::Constant(_), a, k2 @ Expr::Constant(_))
                    if op.is_commutative() && op.is_associative() =>
                {
                    let k = reduce_binary_expr(op1, k1, k2);
                    (a, k)
                }
                // if the expression looks like (a o k1) o k2, we can do:
                //  (a o k1) o k2
                //= a o (k1 o k2) ; associativity
                // (let k = k1 o k2)
                //= a o k
                (a, k1 @ Expr::Constant(_), k2 @ Expr::Constant(_)) if op.is_associative() => {
                    let k = reduce_binary_expr(op1, k1, k2);
                    (a, k)
                }
                // otherwise, rearranging in any way won't reduce the amount of operations/memory
                // needed
                (a, b, c) => (
                    Expr::Binary {
                        operator: op1,
                        lhs: Box::new(a),
                        rhs: Box::new(b),
                        branch_depends_on_result: false,
                    },
                    c,
                ),
            }
        }

        // we are looking into an expression in the form a o (b o c)
        (
            a,
            Expr::Binary {
                operator: op1,
                lhs: b,
                rhs: c,
                branch_depends_on_result: false,
            },
        ) if op == op1 => {
            let (b, c) = reorder_binary_expr(op, *b, *c);
            match (a, b, c) {
                // if we have something in the form:
                // k1 o (k2 o a)
                // we can manipulate it:
                //      k1 o (k2 o a)
                //  =   (k1 o k2) o a ; associativity
                // let k = k1 o k2;
                //   = k o a
                // if the operator is commutative we can even do a o k and reduce the memory needed
                (k1 @ Expr::Constant(_), k2 @ Expr::Constant(_), a) if op.is_associative() => {
                    let k = reduce_binary_expr(op1, k1, k2);
                    if op.is_commutative() {
                        (a, k)
                    } else {
                        (k, a)
                    }
                }
                // if we have sometihng in the form:
                // k1 o (a o k2)
                //  = k1 o (k2 o a) ; commutative
                //  = (k1 o k2) o a ; associative
                //  = a o (k1 o k2) ; commutative
                (k1 @ Expr::Constant(_), a, k2 @ Expr::Constant(_))
                    if op.is_commutative() && op.is_associative() =>
                {
                    let k = reduce_binary_expr(op1, k1, k2);
                    (a, k)
                }
                // now we don't have ony constants on the right side, so the only one left
                // is k o (a o b), which we can put on the other side if the operator is
                // associative.
                (k @ Expr::Constant(_), a, b) if op.is_commutative() => (
                    Expr::Binary {
                        operator: op1,
                        lhs: Box::new(a),
                        rhs: Box::new(b),
                        branch_depends_on_result: false,
                    },
                    k,
                ),
                // otherwise, there's really nothing to do.
                (a, b, c) => (
                    a,
                    Expr::Binary {
                        operator: op1,
                        lhs: Box::new(b),
                        rhs: Box::new(c),
                        branch_depends_on_result: false,
                    },
                ),
            }
        }
        // now, if the constant is in the wrong place we can fix it
        (k @ Expr::Constant(_), b) if op.is_commutative() => (b, k),
        // otherwise, we can't do anything
        (a, b) => (a, b),
    }
}

fn reduce_and_reorder_expr(expr: Expr) -> Expr {
    match expr {
        // first reorder, then reduce
        Expr::Binary {
            operator, lhs, rhs, ..
        } => {
            let (lhs, rhs) = reorder_binary_expr(operator, *lhs, *rhs);
            reduce_binary_expr(operator, lhs, rhs)
        }
        e => reduce_expr(e),
    }
}

fn reduce_binary_expr(operator: BinaryOp, lhs: Expr, rhs: Expr) -> Expr {
    match (reduce_expr(lhs), reduce_expr(rhs)) {
        // two constants can be reduce further with their operator
        (Expr::Constant(a), Expr::Constant(b)) => Expr::Constant(match operator {
            // don't know if this has been handled yet.
            // TODO(#6): handle lvalue errors before getting into compilation
            BinaryOp::Assignment { .. } => {
                panic!("assign operator can't be between two constants")
            }
            BinaryOp::Bit(bitop) => match bitop {
                BitOp::And => a & b,
                BitOp::Or => a | b,
                BitOp::LeftShift => a.wrapping_shl(b as u32),
                BitOp::RightShift => a.wrapping_shr(b as u32),
                BitOp::Xor => a ^ b,
            },
            BinaryOp::Arithmetic(arithmetic_op) => match arithmetic_op {
                // C-style operations
                ArithmeticOp::Add => a.wrapping_add(b),
                ArithmeticOp::Divide => {
                    // NOTE: I could just return the `Divide` operation to be compiled without
                    // evaluating it if we're on integer division.
                    if b == 0 {
                        panic!("in constant expression: division by zero (`{} / {}`)", a, b)
                    } else {
                        a / b
                    }
                }
                ArithmeticOp::Subtract => a.wrapping_sub(b),
                ArithmeticOp::Multiply => a.wrapping_mul(b),
                ArithmeticOp::Modulo => {
                    if b == 0 {
                        panic!("in constant expression: modulo division by zero")
                    } else {
                        a % b
                    }
                }
            },
            BinaryOp::Logic(logicop) => match logicop {
                LogicOp::And => {
                    if a != 0 && b != 0 {
                        1
                    } else {
                        0
                    }
                }
                LogicOp::Or => {
                    if a != 0 || b != 0 {
                        1
                    } else {
                        0
                    }
                }
            },
            BinaryOp::Relational(relation) => match relation {
                crate::ast::Relational::Less => {
                    if a < b {
                        1
                    } else {
                        0
                    }
                }
                crate::ast::Relational::LessEqual => {
                    if a <= b {
                        1
                    } else {
                        0
                    }
                }
                crate::ast::Relational::Greater => {
                    if a > b {
                        1
                    } else {
                        0
                    }
                }
                crate::ast::Relational::GreaterEqual => {
                    if a >= b {
                        1
                    } else {
                        0
                    }
                }
                crate::ast::Relational::Equals => {
                    if a == b {
                        1
                    } else {
                        0
                    }
                }
                crate::ast::Relational::NotEquals => {
                    if a != b {
                        1
                    } else {
                        0
                    }
                }
            },
        }),
        // otherwise, just pack them again
        (a, b) => Expr::Binary {
            operator,
            lhs: Box::new(a),
            rhs: Box::new(b),
            branch_depends_on_result: false,
        },
    }
}

fn reduce_expr(expr: Expr) -> Expr {
    match expr {
        Expr::AlreadyInTarget => expr,
        Expr::Variable { .. } => expr, // cannot reduce a variable lookup
        Expr::Constant(_) => expr,     // cannot reduce a numeric constant further
        Expr::Binary {
            operator, lhs, rhs, ..
        } => reduce_binary_expr(operator, *lhs, *rhs),
        // again, if reducing the expression results in a number, we'll apply the unary operation,
        // otherwise we'll pack it again
        Expr::Unary { operator, inner } => match reduce_expr(*inner) {
            Expr::Constant(c) => Expr::Constant(match operator {
                UnaryOp::Negate => -c,
                UnaryOp::BitNot => !c,
                UnaryOp::LogicNot => {
                    if c == 0 {
                        1
                    } else {
                        0
                    }
                }
            }),
            e => Expr::Unary {
                operator,
                inner: Box::new(e),
            },
        },
    }
}
