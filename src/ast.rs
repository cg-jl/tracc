use crate::lexer::Operator;
use crate::parser::*;

#[derive(Debug)]
pub struct Program(pub Function);

impl Parse for Program {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
        Function::parse(parser).map(Program)
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub return_expr: Expr,
}

#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    BitNot,
    LogicNot,
}

impl UnaryOp {
    pub const fn from_operator(op: Operator) -> Option<Self> {
        Some(match op {
            Operator::Minus => Self::Negate,
            Operator::ExclamationMark => Self::LogicNot,
            Operator::Tilde => Self::BitNot,
            // NOTE: star operator will have to be enabled for pointer access
            Operator::Plus
            | Operator::Star
            | Operator::Slash
            | Operator::DoubleAnd
            | Operator::DoublePipe => return None,
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
}

impl BinaryOp {
    pub const fn precedence(self) -> u8 {
        match self {
            Self::LogicAnd => 4,
            Self::LogicOr => 3,
            Self::Add | Self::Subtract => 8,
            Self::Multiply | Self::Divide => 9,
        }
    }
    pub const fn from_operator(op: Operator) -> Option<Self> {
        Some(match op {
            Operator::Minus => Self::Subtract,
            Operator::Plus => Self::Add,
            Operator::Star => Self::Multiply,
            Operator::Slash => Self::Divide,
            Operator::DoubleAnd => Self::LogicAnd,
            Operator::DoublePipe => Self::LogicOr,
            Operator::Tilde | Operator::ExclamationMark => return None,
        })
    }
}

#[cfg(test)]
mod tests {
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
