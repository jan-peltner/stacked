use core::ops::{Add, Div, Mul, Sub};
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::{array, process::exit};

const STACK_SIZE: usize = 1024;

pub type Program = Vec<Inst>;

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
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
    Psh(Value), // pushes `Value`
    Add,        // pops two values and pushes sum
    Sub,        // pops two values and pushes diff
    Mul,        // pops two values and pushes prod
    Div,        // pops two values and pushes quot
    Adk,        // pushes sum without consuming values
    Prt,        // prints the stack
    Jmp(usize), // jumps to absolute, zero-indexed instruction
    Hlt,        // halts machine
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
                    Inst::Psh(operand) => {
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
                    Inst::Adk => {
                        self.check_stack_underflow(2);
                        self.check_stack_overflow(1);

                        self.stack[self.sp] = self.stack[self.sp - 2] + self.stack[self.sp - 1];

                        self.sp += 1;
                        self.ip += 1;
                    }
                    Inst::Prt => {
                        self.print_stack();

                        self.ip += 1;
                    }
                    Inst::Jmp(addr) => {
                        if *addr > self.insts.len() - 1 {
                            eprintln!("[ERROR] Illegal instruction");
                            exit(1);
                        }

                        self.ip = *addr;
                    }
                    Inst::Hlt => {
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

    fn collect_stack_without_zeroes(svm: Svm) -> Vec<Value> {
        svm.stack
            .into_iter()
            .filter(|num| num > &Value::Int(0.into()))
            .collect()
    }

    #[test]
    fn single_psh() {
        let program = vec![Inst::Psh(10.into())];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 10.into());
    }

    #[test]
    fn single_add() {
        let program = vec![Inst::Psh(10.into()), Inst::Psh(15.into()), Inst::Add];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 25.into());
    }

    #[test]
    fn single_adk() {
        let program = vec![Inst::Psh(10.into()), Inst::Psh(15.into()), Inst::Adk];

        let svm = setup_and_run(program);

        assert_eq!(
            collect_stack_without_zeroes(svm),
            vec![Value::Int(10), Value::Int(15), Value::Int(25)]
        );
    }

    #[test]
    fn single_sub() {
        let program = vec![Inst::Psh(15.into()), Inst::Psh(10.into()), Inst::Sub];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 5.into());
    }

    #[test]
    fn single_mul() {
        let program = vec![Inst::Psh(2.into()), Inst::Psh(10.into()), Inst::Mul];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 20.into());
    }

    #[test]
    fn single_div() {
        let program = vec![Inst::Psh(15.into()), Inst::Psh(3.into()), Inst::Div];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 5.into());
    }
}
