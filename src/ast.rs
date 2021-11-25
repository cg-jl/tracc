use crate::lexer::Operator;
use std::fmt;

// TODO: spans

#[derive(Debug)]
pub struct Program(pub Function);

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Block,
}

//#[derive(Debug)]
pub struct Block(pub Vec<Statement>);

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut dbg_struct = f.debug_struct("Block");
        for (i, stmt) in self.0.iter().enumerate() {
            dbg_struct.field(&i.to_string(), stmt);
        }
        dbg_struct.finish()
    }
}

#[derive(Debug)]
pub enum Statement {
    Return(Expr),
    SingleExpr(Expr),
    DeclareVar { name: String, init: Option<Expr> }, // TODO: add multiple vars
}

impl Default for Statement {
    fn default() -> Self {
        Self::Return(Expr::Constant(0))
    }
}

#[derive(Debug)]
pub struct Identifier(pub String);

// NOTE: should I make a processed expr type?
#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    // TODO(#1): convert variables to &'source str instead of using strings (`Parse<'source>`)
    Variable(VariableKind),
    Constant(i32),
    Unary {
        operator: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        operator: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

impl Expr {
    pub fn is_writable(&self) -> bool {
        matches!(self, Self::Variable(_))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum VariableKind {
    Unprocessed(String),        // TODO: this must be &'source str later
    Processed { index: usize }, // NOTE: currently all variables are ints.
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    BitNot,
    LogicNot,
}

impl UnaryOp {
    pub const fn from_operator((op, _): (Operator, bool)) -> Option<Self> {
        Some(match op {
            Operator::Minus => Self::Negate,
            Operator::ExclamationMark => Self::LogicNot,
            Operator::Tilde => Self::BitNot,
            // NOTE: star operator will have to be enabled for pointer access
            Operator::Plus
            | Operator::Star
            | Operator::Slash
            | Operator::DoubleAnd
            | Operator::DoublePipe
            | Operator::AngleLeft
            | Operator::AngleRight
            | Operator::ExclamationEquals
            | Operator::Equals
            | Operator::DoubleEquals => return None,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LogicAnd,
    LogicOr,
    Relational(Relational),
    Equality(Equality),
    Assign,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Associativity {
    Right,
    Left,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Relational {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Equality {
    Equals,
    NotEquals,
}

impl Equality {
    pub const fn to_condition(self) -> crate::assembly::Condition {
        use crate::assembly::Condition;
        match self {
            Self::Equals => Condition::Equals,
            Self::NotEquals => Condition::NotEquals,
        }
    }
}

impl Relational {
    pub const fn to_condition(self) -> crate::assembly::Condition {
        use crate::assembly::Condition;
        match self {
            Self::Less => Condition::LessThan,
            Self::LessEqual => Condition::LessEqual,
            Self::Greater => Condition::GreaterThan,
            Self::GreaterEqual => Condition::GreaterEqual,
        }
    }
}

impl BinaryOp {
    pub const fn associativity(self) -> Associativity {
        match self {
            Self::LogicAnd
            | Self::LogicOr
            | Self::Add
            | Self::Subtract
            | Self::Multiply
            | Self::Divide
            | Self::Relational(_)
            | Self::Equality(_) => Associativity::Left,
            Self::Assign => Associativity::Right,
        }
    }
    pub const fn precedence(self) -> u8 {
        match self {
            Self::LogicAnd => 4,
            Self::LogicOr => 3,
            Self::Add | Self::Subtract => 11,
            Self::Multiply | Self::Divide => 12,
            Self::Relational(_) => 9,
            Self::Assign => 14, // NOTE: assignment is RTL!
            Self::Equality(_) => 8,
        }
    }
    pub const fn from_operator((op, has_equal): (Operator, bool)) -> Option<Self> {
        Some(match op {
            Operator::Minus => Self::Subtract,
            Operator::Plus => Self::Add,
            Operator::Star => Self::Multiply,
            Operator::Slash => Self::Divide,
            Operator::DoubleAnd => Self::LogicAnd,
            Operator::DoublePipe => Self::LogicOr,
            Operator::AngleLeft => {
                if has_equal {
                    Self::Relational(Relational::LessEqual)
                } else {
                    Self::Relational(Relational::Less)
                }
            }
            Operator::AngleRight => {
                if has_equal {
                    Self::Relational(Relational::GreaterEqual)
                } else {
                    Self::Relational(Relational::Greater)
                }
            }
            Operator::Equals => Self::Assign,
            Operator::DoubleEquals => Self::Equality(Equality::Equals),
            Operator::ExclamationEquals => Self::Equality(Equality::NotEquals),
            Operator::Tilde | Operator::ExclamationMark => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{Parse, ParseRes, Parser};

    use super::*;

    fn parse_test<T: Parse>(source: &str) -> ParseRes<T> {
        use crate::error::SourceMetadata;
        let meta = SourceMetadata::new(source).with_file(std::path::PathBuf::from("<test input>"));
        let mut parser = Parser::new(&meta);
        parser.parse()
    }

    fn parse_ok<T: Parse>(source: &str) -> T {
        parse_test(source).expect("expected no failure")
    }

    mod expression {
        use super::*;
        #[test]
        fn number() {
            assert_eq!(parse_ok::<Expr>("12"), Expr::Constant(12));
            assert_eq!(parse_ok::<Expr>("0"), Expr::Constant(0));
        }
        #[test]
        fn unary_op() {
            assert_eq!(
                parse_ok::<Expr>("-1"),
                Expr::Unary {
                    operator: UnaryOp::Negate,
                    expr: Box::new(Expr::Constant(1))
                }
            )
        }
        #[test]
        fn binary_op() {
            assert_eq!(
                parse_ok::<Expr>("1 + 2"),
                Expr::Binary {
                    operator: BinaryOp::Add,
                    lhs: Box::new(Expr::Constant(1)),
                    rhs: Box::new(Expr::Constant(2))
                }
            );
            assert_eq!(
                parse_ok::<Expr>("1 - 2"),
                Expr::Binary {
                    operator: BinaryOp::Subtract,
                    lhs: Box::new(Expr::Constant(1)),
                    rhs: Box::new(Expr::Constant(2))
                }
            );
        }
        #[test]
        fn compound() {
            assert_eq!(
                parse_ok::<Expr>("10 + ~(5 - 4)"),
                Expr::Binary {
                    operator: BinaryOp::Add,
                    lhs: Box::new(Expr::Constant(10)),
                    rhs: Box::new(Expr::Unary {
                        operator: UnaryOp::BitNot,
                        expr: Box::new(Expr::Binary {
                            operator: BinaryOp::Subtract,
                            lhs: Box::new(Expr::Constant(5)),
                            rhs: Box::new(Expr::Constant(4)),
                        })
                    })
                }
            );
        }
    }
}
