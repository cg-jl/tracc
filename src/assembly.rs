use crate::{format_instr, format_instr_args, write_instruction};
// use bitflags::bitflags;
use std::fmt;

// bitflags! {
//     /// The ALU flags available to the assembly
//     struct PState: u8 {
//         /// Negative flag
//         const N = 0b1000;
//         /// Carry flag
//         const C = 0b0100;
//         /// Overflow flag
//         const V = 0b0010;
//         /// Zero flag
//         const Z = 0b0001;
//     }
// }

// impl PState {
//     pub const fn to_immediate(self) -> Data {
//         Data::Immediate(self.bits as u64)
//     }
// }

// NOTE: could do something in the compiler to make
// instructions that may modify state "adds", "subs" etc
// and make decisions to modify or not the state.

#[derive(Debug, Clone)]
pub enum Assembly {
    Directive(Directive),
    Label(String),
    Instruction(Instruction),
    Comment(String),
}

impl fmt::Display for Assembly {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Directive(direct) => write!(f, "\t.{}", direct),
            Self::Instruction(instr) => write!(f, "\t{}", instr),
            Self::Label(name) => write!(f, "{}:", name),
            Self::Comment(comment) => write!(f, "// {}", comment),
        }
    }
}

impl Assembly {
    pub fn map_instruction<F>(mapper: F) -> impl Fn(&mut Self)
    where
        F: Fn(&mut Instruction),
    {
        move |asm| {
            if let Assembly::Instruction(ref mut instruction) = asm {
                mapper(instruction)
            }
        }
    }
}

impl From<Instruction> for Assembly {
    fn from(instr: Instruction) -> Self {
        Self::Instruction(instr)
    }
}

impl From<Branch> for Assembly {
    fn from(branch: Branch) -> Self {
        Self::Instruction(Instruction::Branch(branch))
    }
}

#[derive(Debug, Clone)]
pub enum Directive {
    Global(String),
    Type(String, String),
    Architecture(String),
}

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Global(name) => write!(f, "global {}", name),
            Self::Type(name, t) => write!(f, "type {}, %{}", name, t),
            Self::Architecture(arch) => write!(f, "arch {}", arch),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    /// Return from a function
    Ret,
    /// Move data to a register
    Mov {
        target: Register,
        source: Data,
    },
    /// Move (with applied not) data to a register
    MvN {
        target: Register,
        source: Data,
    },
    /// Compare a register with some data
    Cmp {
        register: Register,
        data: Data,
    },
    /// Sets register 1 or 0 depending on condition
    Cset {
        target: Register,
        condition: Condition,
    },
    /// Negate a register
    Neg {
        target: Register,
        source: Register,
    },
    /// Add a register and a source of data into a register
    Add {
        target: Register,
        lhs: Register,
        rhs: Data,
    },
    /// subtract the contents of a register from some data into a register
    Sub {
        target: Register,
        lhs: Register,
        rhs: Data,
    },
    /// Multiply two numbers (currently not u/s/l)
    Mul {
        target: Register,
        lhs: Register,
        rhs: Data,
    },
    /// Divide two signed numbers
    Div {
        target: Register,
        lhs: Register,
        rhs: Data,
        signed: bool,
    },
    /// Store a register into memory
    Str {
        register: Register,
        address: Memory,
    },
    /// Load a register from memory
    Ldr {
        register: Register,
        address: Memory,
    },
    // /// Branch for different situations
    Branch(Branch),
}

#[derive(Debug, Clone, Copy)]
pub enum Branch {
    /// normal (unconditional) branch, always executed
    Unconditional {
        register: Option<Register>,
        label: Label,
    },
    /// branch with link (aka call)
    Linked { label: Label },
    /// Conditional branch
    Conditional { condition: Condition, label: Label },
}

#[derive(Debug, Clone, Copy)]
pub struct Label {
    num: usize,
}

impl Into<Assembly> for Label {
    fn into(self) -> Assembly {
        Assembly::Label(self.to_string())
    }
}

impl Label {
    /// Creates a new label from its number
    ///
    /// # Safety
    /// The number generated for this label **must** be unique.
    /// Use [`LabelGenerator::new_label`] to get labels.
    pub const unsafe fn new(num: usize) -> Self {
        Self { num }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Ret => write_instruction!(f, "ret"),
            Self::Mov { target, source } => write_instruction!(f, "mov", target, source),
            Self::MvN { target, source } => write_instruction!(f, "mvn", target, source),
            Self::Cmp { register, data } => write_instruction!(f, "cmp", register, data),
            Self::Cset { target, condition } => write_instruction!(f, "cset", target, condition),
            Self::Neg { target, source } => write_instruction!(f, "neg", target, source),
            Self::Add { target, lhs, rhs } => write_instruction!(f, "add", target, lhs, rhs),
            Self::Sub { target, lhs, rhs } => write_instruction!(f, "sub", target, lhs, rhs),
            Self::Str { register, address } => write_instruction!(f, "str", register, address),
            Self::Ldr { register, address } => write_instruction!(f, "ldr", register, address),
            Self::Mul { target, lhs, rhs } => write_instruction!(f, "mul", target, lhs, rhs),
            Self::Div {
                target,
                lhs,
                rhs,
                signed,
            } => write_instruction!(
                f,
                format!("{}div", if *signed { 's' } else { 'u' }),
                target,
                lhs,
                rhs
            ),
            Self::Branch(branch) => branch.fmt(f),
        }
    }
}

impl fmt::Display for Branch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Conditional { condition, label } => {
                write_instruction!(f, format!("b{}", condition), label)
            }
            Self::Unconditional {
                register: Some(register),
                label,
            } => write_instruction!(f, "br", register, label),
            Self::Unconditional {
                register: None,
                label,
            } => write_instruction!(f, "b", label),
            Self::Linked { label } => write_instruction!(f, "bl", label),
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ".L{}", self.num)
    }
}

#[macro_export]
macro_rules! format_instr_args {
    () => { "" };
    ($arg:expr) => { "{}" };
    ($first:expr, $($rest:expr),+) => {
        concat!("{}, ", format_instr_args!($($rest),+))
    }
}

#[macro_export]
macro_rules! format_instr {
    ($name:expr) => { format_args!("{}", $name) };
    ($name:expr, $($args:expr),+) => {
        format_args!(concat!("{:3} ", format_instr_args!($($args),+)), $name, $($args),+)
    };
}

#[macro_export]
macro_rules! write_instruction {
    ($formatter:expr, $name:expr) => {
        $formatter.write_fmt(format_instr!($name))
    };
    ($formatter:expr, $name:expr, $($args:expr),+) => { $formatter.write_fmt(format_instr!($name, $($args),+)) }
}

impl Instruction {
    pub fn map_over_memory<F>(mapper: F) -> impl Fn(&mut Self)
    where
        F: Fn(&mut Memory),
    {
        move |instr| match instr {
            Self::Str {
                ref mut address, ..
            } => mapper(address),
            Self::Ldr {
                ref mut address, ..
            } => mapper(address),
            Self::Add { .. }
            | Self::Mov { .. }
            | Self::Sub { .. }
            | Self::Neg { .. }
            | Self::Cset { .. }
            | Self::MvN { .. }
            | Self::Cmp { .. }
            | Self::Mul { .. }
            | Self::Div { .. }
            | Self::Branch(_)
            | Self::Ret => {}
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Data {
    /// Take data from register
    Register(Register),
    /// Take data from an immediate value
    Immediate(u64),
    /// Stack offset
    StackOffset(u64),
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Immediate(value) => write!(f, "#{}", value),
            Self::Register(reg) => write!(f, "{}", reg),
            Self::StackOffset(_) => unreachable!("stack offsets must be determined before"),
        }
    }
}

impl Data {
    pub const fn immediate(immediate: u64, bit_size: BitSize) -> Self {
        if immediate == 0 {
            Self::Register(Register::ZeroRegister { bit_size })
        } else {
            Self::Immediate(immediate)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Condition {
    /// Previous comparison resulted in equal
    Equals,
    /// Previous comparison resulted in not equal
    NotEquals,
    GreaterThan,
    GreaterEqual,
    LessThan,
    LessEqual,
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Equals => write!(f, "eq"),
            Self::NotEquals => write!(f, "ne"),
            Self::LessEqual => write!(f, "le"),
            Self::LessThan => write!(f, "lt"),
            Self::GreaterThan => write!(f, "gt"),
            Self::GreaterEqual => write!(f, "ge"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Register {
    /// Unnamed general purpose register
    GeneralPurpose { index: u8, bit_size: BitSize },
    /// Register which ignores writes and gives zero when read
    ZeroRegister { bit_size: BitSize },
    /// The stack pointer
    StackPointer,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::GeneralPurpose { index, bit_size } => write!(f, "{}{}", bit_size.char(), index),
            Self::ZeroRegister { bit_size } => write!(f, "{}zr", bit_size.char()),
            Self::StackPointer => write!(f, "sp"),
        }
    }
}

impl Register {
    pub const fn bit_size(self) -> BitSize {
        match self {
            Self::GeneralPurpose { bit_size, .. } => bit_size,
            Self::ZeroRegister { bit_size } => bit_size,
            Self::StackPointer => BitSize::Bit64,
        }
    }
}

/// wether the register is 32 or 64 bit (to use 'w' or 'x')
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BitSize {
    Bit32,
    Bit64,
}

impl BitSize {
    fn char(&self) -> char {
        match self {
            Self::Bit32 => 'w',
            Self::Bit64 => 'x',
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Memory {
    pub register: Register,
    pub offset: Offset,
}

impl fmt::Display for Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let offset = self.offset.determined_size();
        if offset == 0 {
            write!(f, "[{}]", self.register)
        } else {
            write!(f, "[{}, {}]", self.register, Data::Immediate(offset as u64))
        }
    }
}

impl Memory {
    pub fn partition(self, block_size: usize) -> impl Iterator<Item = Self> {
        let Memory { register, offset } = self;
        offset
            .partition(block_size)
            .map(move |offset| Memory { register, offset })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Offset {
    /// offset that is added directly to the register
    Determined(usize),
    /// offset that is not yet fully determined and needs
    /// finalization of some context
    Undetermined(usize),
}

fn partition(block_size: usize, mut offset_begin: usize) -> impl Iterator<Item = usize> {
    std::iter::from_fn(move || {
        let curr = offset_begin;
        offset_begin += block_size;
        Some(curr)
    })
}

impl Offset {
    pub fn partition(self, block_size: usize) -> Box<dyn Iterator<Item = Self>> {
        match self {
            Self::Determined(begin) => Box::new(
                partition(block_size, 0)
                    .take_while(move |x| *x <= begin)
                    .map(Self::Determined),
            ),
            Self::Undetermined(begin) => {
                Box::new(partition(block_size, begin).map(Self::Undetermined))
            }
        }
    }
    fn determined_size(&self) -> usize {
        match self {
            Self::Determined(det) => *det,
            _ => unreachable!(),
        }
    }
}
