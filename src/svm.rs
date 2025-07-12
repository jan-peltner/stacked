use core::ops::{Add, Div, Mul, Sub};
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::{array, process::exit};

const STACK_SIZE: usize = 10;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Int(i64),
    Float(f64),
}

impl From<i64> for Value {
    fn from(val: i64) -> Self {
        Self::Int(val)
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Self::Float(val)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            &Value::Int(val) => {
                write!(f, "{:.4}", val)?;
            }
            &Value::Float(val) => {
                write!(f, "{}", val)?;
            }
        }
        Ok(())
    }
}

impl Add for Value {
    type Output = Value;
    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Value::Int(lhs_val) => match rhs {
                Value::Int(rhs_val) => (lhs_val + rhs_val).into(),
                Value::Float(rhs_val) => (lhs_val as f64 + rhs_val).into(),
            },
            Value::Float(lhs_val) => match rhs {
                Value::Int(rhs_val) => (lhs_val + rhs_val as f64).into(),
                Value::Float(rhs_val) => (lhs_val + rhs_val).into(),
            },
        }
    }
}

impl Sub for Value {
    type Output = Value;
    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Value::Int(lhs_val) => match rhs {
                Value::Int(rhs_val) => (lhs_val - rhs_val).into(),
                Value::Float(rhs_val) => (lhs_val as f64 - rhs_val).into(),
            },
            Value::Float(lhs_val) => match rhs {
                Value::Int(rhs_val) => (lhs_val - rhs_val as f64).into(),
                Value::Float(rhs_val) => (lhs_val - rhs_val).into(),
            },
        }
    }
}
impl Mul for Value {
    type Output = Value;
    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Value::Int(lhs_val) => match rhs {
                Value::Int(rhs_val) => (lhs_val * rhs_val).into(),
                Value::Float(rhs_val) => (lhs_val as f64 * rhs_val).into(),
            },
            Value::Float(lhs_val) => match rhs {
                Value::Int(rhs_val) => (lhs_val * rhs_val as f64).into(),
                Value::Float(rhs_val) => (lhs_val * rhs_val).into(),
            },
        }
    }
}
impl Div for Value {
    type Output = Value;
    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Value::Int(lhs_val) => match rhs {
                Value::Int(rhs_val) => (lhs_val / rhs_val).into(),
                Value::Float(rhs_val) => (lhs_val as f64 / rhs_val).into(),
            },
            Value::Float(lhs_val) => match rhs {
                Value::Int(rhs_val) => (lhs_val / rhs_val as f64).into(),
                Value::Float(rhs_val) => (lhs_val / rhs_val).into(),
            },
        }
    }
}

#[derive(Debug)]
pub enum Inst {
    Push(Value),
    Add,
    Sub,
    Mul,
    Div,
    Print, // prints the stack
    Jump(usize),
    Halt,
}

#[derive(Debug)]
pub struct Svm {
    pub ip: usize,
    pub sp: usize,
    pub stack: [Value; STACK_SIZE],
    pub insts: Vec<Inst>,
}

impl Svm {
    pub fn from_program(program: Vec<Inst>) -> Self {
        Svm {
            ip: 0,
            sp: 0,
            stack: array::from_fn(|_| 0.into()),
            insts: program,
        }
    }

    pub fn run(&mut self) {
        while self.ip < self.insts.len() {
            if let Some(inst) = self.insts.get(self.ip) {
                match inst {
                    Inst::Push(operand) => {
                        self.check_stack_overflow(1);

                        self.stack[self.sp] = *operand;

                        self.sp += 1;
                        self.ip += 1;
                    }
                    Inst::Add => {
                        self.check_stack_underflow(2);

                        self.stack[self.sp - 2] = self.stack[self.sp - 2] + self.stack[self.sp - 1];

                        self.sp -= 1;
                        self.ip += 1;
                    }
                    Inst::Sub => {
                        self.check_stack_underflow(2);
                        self.stack[self.sp - 2] = self.stack[self.sp - 2] - self.stack[self.sp - 1];
                        self.sp -= 1;
                        self.ip += 1;
                    }
                    Inst::Mul => {
                        self.check_stack_underflow(2);
                        self.stack[self.sp - 2] = self.stack[self.sp - 2] * self.stack[self.sp - 1];
                        self.sp -= 1;
                        self.ip += 1;
                    }
                    Inst::Div => {
                        self.check_stack_underflow(2);
                        self.stack[self.sp - 2] = self.stack[self.sp - 2] / self.stack[self.sp - 1];
                        self.sp -= 1;
                        self.ip += 1;
                    }
                    Inst::Print => {
                        self.print_stack();

                        self.ip += 1;
                    }
                    Inst::Jump(addr) => {
                        if *addr > self.insts.len() - 1 {
                            eprintln!("[ERROR] Illegal instruction");
                            exit(1);
                        }

                        self.ip = *addr;
                    }
                    Inst::Halt => {
                        println!("[INFO] StackedVM halted");
                        exit(0);
                    }
                }
            }
        }
    }

    fn check_stack_overflow(&self, count: usize) {
        if self.sp + count > STACK_SIZE {
            eprintln!("[ERROR] Stack overflow");
            exit(1);
        }
    }

    fn check_stack_underflow(&self, count: usize) {
        if self.sp < count {
            eprintln!("[ERROR] Stack underflow");
            exit(1);
        }
    }

    fn print_stack(&self) {
        println!("[INFO] Stack:");
        for i in 0..self.sp {
            println!("    [{i}] => {}", self.stack[i]);
        }
        println!("    [{}] <-- sp", self.sp)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn setup_and_run(program: Vec<Inst>) -> Svm {
        let mut svm = Svm::from_program(program);
        svm.run();
        return svm;
    }

    #[test]
    fn single_push() {
        let program = vec![Inst::Push(10.into())];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 10.into());
    }

    #[test]
    fn single_add() {
        let program = vec![Inst::Push(10.into()), Inst::Push(15.into()), Inst::Add];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 25.into());
    }

    #[test]
    fn single_sub() {
        let program = vec![Inst::Push(15.into()), Inst::Push(10.into()), Inst::Sub];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 5.into());
    }

    #[test]
    fn single_mul() {
        let program = vec![Inst::Push(2.into()), Inst::Push(10.into()), Inst::Mul];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 20.into());
    }

    #[test]
    fn single_div() {
        let program = vec![Inst::Push(15.into()), Inst::Push(3.into()), Inst::Div];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 5.into());
    }
}
