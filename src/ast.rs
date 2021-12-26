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
    /// Dummy that signifies that the expression you want to compile is already in the target
    AlreadyInTarget,
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

// NOTE: unary operators also have different predecences (14 or 13, depending on them), it's just
// that all the ones that we have are on the same group.
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
            | Operator::And
            | Operator::DoubleAngleLeft
            | Operator::DoubleAngleRight
            | Operator::Hat
            | Operator::Pipe
            | Operator::Slash
            | Operator::DoubleAnd
            | Operator::DoublePipe
            | Operator::AngleLeft
            | Operator::AngleRight
            | Operator::Percentage
            | Operator::Equals => return None,
        })
    }
}

/// Includes anything that is related to basic arithmetic
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithmeticOp {
    /// `+` operator
    Add,
    /// `-` (binary) operator
    Subtract,
    /// '*' (binary) operator
    Multiply,
    /// `/` operator
    Divide,
    /// `%` operator
    Modulo,
}

/// Includes all bit operations that can be done with simple registers (not SSE for the moment)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitOp {
    /// `&` operator
    And,
    /// `|` operator
    Or,
    /// `^` operator
    Xor,
    /// `>>` operator
    RightShift,
    /// `<<` operator
    LeftShift,
}

/// Includes the classic shortcuts for if statements: `&&` and `||`. These operate based on
/// equality to 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicOp {
    And,
    Or,
}

/// Includes any kind of operator that needs two values
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Arithmetic(ArithmeticOp),
    Logic(LogicOp),
    Bit(BitOp),
    Relational(Relational),
    Assignment { op: Option<AssignmentEnabledOp> },
}

/// Includes all the operators that are accepted by assignment
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentEnabledOp {
    Arithmetic(ArithmeticOp),
    Logic(LogicOp),
    Bit(BitOp),
}

impl const From<AssignmentEnabledOp> for BinaryOp {
    fn from(op: AssignmentEnabledOp) -> Self {
        match op {
            AssignmentEnabledOp::Arithmetic(a) => Self::Arithmetic(a),
            AssignmentEnabledOp::Bit(b) => Self::Bit(b),
            AssignmentEnabledOp::Logic(l) => Self::Logic(l),
        }
    }
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
    Equals,
    NotEquals,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Equality {
    Equals,
    NotEquals,
}

// convert easily to AssignmentEnabledOp
impl const From<ArithmeticOp> for AssignmentEnabledOp {
    #[inline]
    fn from(a: ArithmeticOp) -> Self {
        Self::Arithmetic(a)
    }
}

impl const From<LogicOp> for AssignmentEnabledOp {
    #[inline]
    fn from(l: LogicOp) -> Self {
        Self::Logic(l)
    }
}

impl const Into<AssignmentEnabledOp> for BitOp {
    #[inline]
    fn into(self) -> AssignmentEnabledOp {
        AssignmentEnabledOp::Bit(self)
    }
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
            Self::Equals => Condition::Equals,
            Self::NotEquals => Condition::NotEquals,
        }
    }
    pub const fn precedence(self) -> u8 {
        match self {
            Self::Less | Self::LessEqual | Self::Greater | Self::GreaterEqual => 15 - 6,
            Self::Equals | Self::NotEquals => 15 - 7,
        }
    }
}

impl BitOp {
    pub const fn is_commutative(self) -> bool {
        matches!(self, Self::And | Self::Or | Self::Xor)
    }
    pub const fn precedence(self) -> u8 {
        match self {
            Self::LeftShift | Self::RightShift => 15 - 5,
            Self::And => 15 - 8,
            Self::Or => 15 - 10,
            Self::Xor => 15 - 9,
        }
    }
}

impl ArithmeticOp {
    pub const fn precedence(self) -> u8 {
        match self {
            Self::Add | Self::Subtract => 15 - 4,
            Self::Multiply | Self::Divide | Self::Modulo => 15 - 3,
        }
    }
    /// if the operation is commutative, then the constants can be swapped around and use
    /// immediates. If not.... well, we can't :)
    pub const fn supports_commutativity(self) -> bool {
        matches!(self, Self::Add | Self::Multiply)
    }
}

impl LogicOp {
    pub const fn precedence(self) -> u8 {
        match self {
            Self::And => 15 - 11,
            Self::Or => 15 - 12,
        }
    }
}

impl BinaryOp {
    pub const fn associativity(self) -> Associativity {
        match self {
            Self::Arithmetic(_) | Self::Logic(_) | Self::Bit(_) | Self::Relational(_) => {
                Associativity::Left
            }
            Self::Assignment { .. } => Associativity::Right,
        }
    }
    pub const fn precedence(self) -> u8 {
        match self {
            Self::Arithmetic(arithm) => arithm.precedence(),
            Self::Logic(logic) => logic.precedence(),
            Self::Bit(bit) => bit.precedence(),
            Self::Relational(relation) => relation.precedence(),
            // all assignments have the same precedence, regardless of the operator it might come
            // with
            Self::Assignment { .. } => 15 - 14,
        }
    }
    pub const fn from_operator((op, has_equal): (Operator, bool)) -> Option<Self> {
        Some(match op {
            Operator::Plus => {
                let op = ArithmeticOp::Add;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Arithmetic(op)
                }
            }
            Operator::Minus => {
                let op = ArithmeticOp::Subtract;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Arithmetic(op)
                }
            }
            Operator::ExclamationMark => {
                // that is a unary not
                if !has_equal {
                    return None;
                } else {
                    Self::Relational(Relational::NotEquals)
                }
            }
            Operator::Tilde => return None, // tilde is a unary operator
            // a star when a binary operator is needed is clearly a multiplication
            Operator::Star => {
                let op = ArithmeticOp::Multiply;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Arithmetic(op)
                }
            }
            Operator::Slash => {
                let op = ArithmeticOp::Divide;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Arithmetic(op)
                }
            }
            Operator::DoubleAnd => {
                let op = LogicOp::And;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Logic(op)
                }
            }
            Operator::DoublePipe => {
                let op = LogicOp::Or;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Logic(op)
                }
            }
            Operator::DoubleAngleRight => {
                let op = BitOp::RightShift;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Bit(op)
                }
            }
            Operator::DoubleAngleLeft => {
                let op = BitOp::LeftShift;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Bit(op)
                }
            }
            Operator::AngleRight => Self::Relational(if has_equal {
                Relational::GreaterEqual
            } else {
                Relational::Greater
            }),
            Operator::AngleLeft => Self::Relational(if has_equal {
                Relational::LessEqual
            } else {
                Relational::Less
            }),
            Operator::Percentage => {
                let op = ArithmeticOp::Modulo;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Arithmetic(op)
                }
            }
            Operator::Equals => {
                if has_equal {
                    Self::Relational(Relational::Equals)
                } else {
                    Self::Assignment { op: None }
                }
            }
            Operator::And => {
                let op = BitOp::And;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Bit(op)
                }
            },
            Operator::Pipe => {
                let op = BitOp::Or;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Bit(op)
                }
            },
            Operator::Hat => {
                let op = BitOp::Xor;
                if has_equal {
                    Self::Assignment {
                        op: Some(op.into()),
                    }
                } else {
                    Self::Bit(op)
                }
            }
            // Operator::Minus => Self::Subtract,
            // Operator::Plus => Self::Add,
            // Operator::Star => Self::Multiply,
            // Operator::Slash => Self::Divide,
            // Operator::DoubleAnd => Self::LogicAnd,
            // Operator::DoublePipe => Self::LogicOr,
            // Operator::AngleLeft => {
            //     if has_equal {
            //         Self::Relational(Relational::LessEqual)
            //     } else {
            //         Self::Relational(Relational::Less)
            //     }
            // }
            // Operator::AngleRight => {
            //     if has_equal {
            //         Self::Relational(Relational::GreaterEqual)
            //     } else {
            //         Self::Relational(Relational::Greater)
            //     }
            // }
            // Operator::Equals => Self::Assign,
            // Operator::DoubleEquals => Self::Equality(Equality::Equals),
            // Operator::ExclamationEquals => Self::Equality(Equality::NotEquals),
            // Operator::Tilde | Operator::ExclamationMark => return None,
        })
    }
}
