//! Internal AST used for codegen, once all the checks have passed
//! After this AST is generated, no more (user) errors can happen
use crate::ast::{BinaryOp, UnaryOp};

#[derive(Debug)]
#[repr(transparent)]
pub struct Program<'source>(pub Function<'source>);

#[derive(Debug)]
pub struct Function<'source> {
    /// The name of the function, needed for the label
    pub name: &'source str,
    /// the body of the function
    pub body: Vec<Statement>,
    /// the amount of variables that need to be allocated
    pub var_amt: usize,
}

#[derive(Debug)]
pub enum Statement {
    /// Return from the function with a value
    Return(Expr),
    /// Just run it.
    Single(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Variable {
        index: usize,
    },
    Constant(i32),
    Unary {
        operator: UnaryOp,
        inner: Box<Expr>,
    },
    Binary {
        operator: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    /// Dummy that signals that the expression to compile was served in the target
    AlreadyInTarget, // NOTE: make a better choice for this in order to ensure safety.
}
