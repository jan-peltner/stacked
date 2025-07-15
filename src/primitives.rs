use core::ops::{Add, Div, Mul, Sub};
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug)]
pub enum Token {
    Symbol(Sym),
    Instruction(Op),
    Directive(Dir),
    DataLiteral(Atom),
    OffsetLiteral(usize),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Symbol(sym) => match sym {
                Sym::Label(label) => write!(f, "Symbol::Label::<{label}>"),
                Sym::Var(var) => write!(f, "Symbol::Var::<{var}>"),
            },
            Token::Instruction(op) => write!(f, "Instruction::<{op}>"),
            Token::Directive(dir) => write!(f, "Directive::<{dir}>"),
            Token::DataLiteral(atom) => match atom {
                Atom::Int(int) => write!(f, "DataLiteral::Atom::Int::<{int}>"),
                Atom::Float(float) => write!(f, "DataLiteral::Atom::Float::<{float}>"),
            },
            Token::OffsetLiteral(usz) => write!(f, "OffsetLiteral::<{usz}>"),
        }
    }
}

#[derive(Debug)]
pub enum Op {
    Push,
    Dupe,

    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,

    Jump,
    Jump1,
    Jump0,

    Loadi,
    Loadf,
    Write,

    Print,
    Halt,
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Op::Push => write!(f, "PUSH"),
            Op::Dupe => write!(f, "DUPE"),
            Op::Add => write!(f, "ADD"),
            Op::Sub => write!(f, "SUB"),
            Op::Mul => write!(f, "MUL"),
            Op::Div => write!(f, "DIV"),
            Op::Eq => write!(f, "EQ"),
            Op::Neq => write!(f, "NEQ"),
            Op::Gt => write!(f, "GT"),
            Op::Gte => write!(f, "GTE"),
            Op::Lt => write!(f, "LT"),
            Op::Lte => write!(f, "LTE"),
            Op::Jump => write!(f, "JUMP"),
            Op::Jump1 => write!(f, "JUMP1"),
            Op::Jump0 => write!(f, "JUMP0"),
            Op::Loadi => write!(f, "LOADI"),
            Op::Loadf => write!(f, "LOADF"),
            Op::Write => write!(f, "WRITE"),
            Op::Print => write!(f, "PRINT"),
            Op::Halt => write!(f, "HALT"),
        }
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Sym {
    Label(String),
    Var(String),
}

#[derive(Debug)]
pub enum Dir {
    Prog,
    Use,
    Data,
}

impl Display for Dir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Dir::Prog => write!(f, "@prog"),
            Dir::Use => write!(f, "@use"),
            Dir::Data => write!(f, "@data"),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Atom {
    Int(i64),
    Float(f64),
}

impl From<i64> for Atom {
    fn from(val: i64) -> Self {
        Self::Int(val)
    }
}

impl From<f64> for Atom {
    fn from(val: f64) -> Self {
        Self::Float(val)
    }
}

impl Atom {
    pub fn spawn_bool_true() -> Self {
        Self::Int(1.into())
    }

    pub fn spawn_bool_false() -> Self {
        Self::Int(0.into())
    }

    pub fn is_true(&self) -> bool {
        match self {
            &Atom::Int(1) => true,
            _ => false,
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            &Atom::Int(val) => {
                write!(f, "{}", val)?;
            }
            &Atom::Float(val) => {
                write!(f, "{}", val)?;
            }
        }
        Ok(())
    }
}

impl Add for Atom {
    type Output = Atom;
    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Atom::Int(lhs_val) => match rhs {
                Atom::Int(rhs_val) => (lhs_val + rhs_val).into(),
                Atom::Float(rhs_val) => (lhs_val as f64 + rhs_val).into(),
            },
            Atom::Float(lhs_val) => match rhs {
                Atom::Int(rhs_val) => (lhs_val + rhs_val as f64).into(),
                Atom::Float(rhs_val) => (lhs_val + rhs_val).into(),
            },
        }
    }
}

impl Sub for Atom {
    type Output = Atom;
    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Atom::Int(lhs_val) => match rhs {
                Atom::Int(rhs_val) => (lhs_val - rhs_val).into(),
                Atom::Float(rhs_val) => (lhs_val as f64 - rhs_val).into(),
            },
            Atom::Float(lhs_val) => match rhs {
                Atom::Int(rhs_val) => (lhs_val - rhs_val as f64).into(),
                Atom::Float(rhs_val) => (lhs_val - rhs_val).into(),
            },
        }
    }
}

impl Mul for Atom {
    type Output = Atom;
    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Atom::Int(lhs_val) => match rhs {
                Atom::Int(rhs_val) => (lhs_val * rhs_val).into(),
                Atom::Float(rhs_val) => (lhs_val as f64 * rhs_val).into(),
            },
            Atom::Float(lhs_val) => match rhs {
                Atom::Int(rhs_val) => (lhs_val * rhs_val as f64).into(),
                Atom::Float(rhs_val) => (lhs_val * rhs_val).into(),
            },
        }
    }
}

impl Div for Atom {
    type Output = Atom;
    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Atom::Int(lhs_val) => match rhs {
                Atom::Int(rhs_val) => (lhs_val / rhs_val).into(),
                Atom::Float(rhs_val) => (lhs_val as f64 / rhs_val).into(),
            },
            Atom::Float(lhs_val) => match rhs {
                Atom::Int(rhs_val) => (lhs_val / rhs_val as f64).into(),
                Atom::Float(rhs_val) => (lhs_val / rhs_val).into(),
            },
        }
    }
}

#[derive(Debug)]
pub enum Inst {
    Push(Atom),  // pushes atom operand
    Dupe(usize), // pushes nth top atom, effectively duplicating

    Add, // consumes two value_atoms and pushes sum
    Sub, // consumes two value_atoms and pushes diff
    Mul, // consumes two value_atoms and pushes prod
    Div, // consumes two value_atoms and pushes quot

    // logical comparisons consume two atoms and push a bool_atom (1 or 0)
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,

    Jump(usize),  // jumps to absolute instruction
    Jump1(usize), // consumes bool_atom and jumps to absolute address if true
    Jump0(usize), // consumes bool_atom and jumps to absolute address if false

    Loadi, // consumes one address_atom, loads int_atom (8 bytes) from address
    Loadf, // consumes one address_atom, loads float_atom (8 bytes) from address
    Write, // consumes two atoms (address_atom + value_atom) and writes it to address

    Print, // prints the stack
    Halt,  // halts machine
}

pub type Program = Vec<Inst>;
