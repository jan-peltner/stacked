use core::ops::{Add, Div, Mul, Sub};
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::{array, process::exit};

const STACK_CAP: usize = 1024;
const DATA_CAP: usize = 1024;

pub type Program = Vec<Inst>;

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
    fn spawn_bool_true() -> Self {
        Self::Int(1.into())
    }

    fn spawn_bool_false() -> Self {
        Self::Int(0.into())
    }

    fn is_true(&self) -> bool {
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
                write!(f, "{:.4}", val)?;
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

#[derive(Debug)]
pub struct Svm {
    ip: usize, // instruction pointer
    sp: usize, // stack pointer
    stack: [Atom; STACK_CAP],
    insts: Vec<Inst>,
    data: [u8; DATA_CAP],
}

impl Svm {
    pub fn from_program(program: Program) -> Self {
        Svm {
            ip: 0,
            sp: 0,
            stack: array::from_fn(|_| 0.into()),
            insts: program,
            data: array::from_fn(|_| 0.into()),
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
                    Inst::Dupe(operand) => {
                        self.check_stack_underflow(*operand);
                        self.check_stack_overflow(1);

                        self.stack[self.sp] = self.stack[self.sp - *operand - 1];

                        self.sp += 1;
                        self.ip += 1;
                    }
                    Inst::Print => {
                        self.print_stack();

                        self.ip += 1;
                    }
                    Inst::Jump(addr) => {
                        self.check_valid_inst_address(*addr);

                        if *addr > self.insts.len() - 1 {
                            eprintln!("[ERROR] Illegal instruction");
                            exit(1);
                        }

                        self.ip = *addr;
                    }
                    Inst::Write => {
                        self.check_stack_underflow(2);

                        if let Atom::Int(addr) = self.stack[self.sp - 2] {
                            Svm::check_valid_data_address(addr);

                            match self.stack[self.sp - 1] {
                                Atom::Int(val) => {
                                    self.write_data(addr as usize, val.to_le_bytes().to_vec());
                                }
                                Atom::Float(val) => {
                                    self.write_data(addr as usize, val.to_le_bytes().to_vec());
                                }
                            }
                        } else {
                            eprintln!("[ERROR] Illegal address (float not allowed)");
                            exit(1);
                        }

                        self.ip += 1;
                        self.sp -= 2;
                    }
                    Inst::Loadi => {
                        self.check_stack_underflow(1);

                        if let Atom::Int(addr) = self.stack[self.sp - 1] {
                            Svm::check_valid_data_address(addr);
                            self.stack[self.sp - 1] = i64::from_le_bytes(
                                self.read_data(addr as usize, 8)
                                    .try_into()
                                    .expect("[ERROR] Could not convert bytes to float"),
                            )
                            .into();
                        } else {
                            eprintln!("[ERROR] Illegal address (float not allowed)");
                            exit(1);
                        }

                        self.ip += 1;
                    }
                    Inst::Loadf => {
                        self.check_stack_underflow(1);

                        if let Atom::Int(addr) = self.stack[self.sp - 1] {
                            Svm::check_valid_data_address(addr);
                            self.stack[self.sp - 1] = f64::from_le_bytes(
                                self.read_data(addr as usize, 8)
                                    .try_into()
                                    .expect("[ERROR] Could not convert bytes to float"),
                            )
                            .into();
                        } else {
                            eprintln!("[ERROR] Illegal address (float not allowed)");
                            exit(1);
                        }

                        self.ip += 1;
                    }
                    Inst::Eq => {
                        self.check_stack_underflow(2);

                        self.stack[self.sp - 2] =
                            if self.stack[self.sp - 2] == self.stack[self.sp - 1] {
                                Atom::spawn_bool_true()
                            } else {
                                Atom::spawn_bool_false()
                            };

                        self.ip += 1;
                        self.sp -= 1;
                    }
                    Inst::Neq => {
                        self.check_stack_underflow(2);

                        self.stack[self.sp - 2] =
                            if self.stack[self.sp - 2] != self.stack[self.sp - 1] {
                                Atom::spawn_bool_true()
                            } else {
                                Atom::spawn_bool_false()
                            };

                        self.ip += 1;
                        self.sp -= 1;
                    }
                    Inst::Gt => {
                        self.check_stack_underflow(2);

                        self.stack[self.sp - 2] =
                            if self.stack[self.sp - 2] > self.stack[self.sp - 1] {
                                Atom::spawn_bool_true()
                            } else {
                                Atom::spawn_bool_false()
                            };

                        self.ip += 1;
                        self.sp -= 1;
                    }
                    Inst::Gte => {
                        self.check_stack_underflow(2);

                        self.stack[self.sp - 2] =
                            if self.stack[self.sp - 2] >= self.stack[self.sp - 1] {
                                Atom::spawn_bool_true()
                            } else {
                                Atom::spawn_bool_false()
                            };

                        self.ip += 1;
                        self.sp -= 1;
                    }
                    Inst::Lt => {
                        self.check_stack_underflow(2);

                        self.stack[self.sp - 2] =
                            if self.stack[self.sp - 2] < self.stack[self.sp - 1] {
                                Atom::spawn_bool_true()
                            } else {
                                Atom::spawn_bool_false()
                            };

                        self.ip += 1;
                        self.sp -= 1;
                    }
                    Inst::Lte => {
                        self.check_stack_underflow(2);

                        self.stack[self.sp - 2] =
                            if self.stack[self.sp - 2] <= self.stack[self.sp - 1] {
                                Atom::spawn_bool_true()
                            } else {
                                Atom::spawn_bool_false()
                            };

                        self.ip += 1;
                        self.sp -= 1;
                    }
                    Inst::Halt => {
                        println!("[INFO] Stacked halted");
                        exit(0);
                    }
                    Inst::Jump1(addr) => {
                        self.check_stack_overflow(1);
                        self.check_valid_inst_address(*addr);

                        let flag = self.stack[self.sp - 1].is_true();
                        if flag {
                            self.ip = *addr;
                        }

                        if !flag {
                            self.ip += 1;
                        }

                        self.sp -= 1;
                    }
                    Inst::Jump0(addr) => {
                        self.check_stack_overflow(1);
                        self.check_valid_inst_address(*addr);

                        let flag = !self.stack[self.sp - 1].is_true();
                        if flag {
                            self.ip = *addr;
                        }

                        if !flag {
                            self.ip += 1;
                        }

                        self.sp -= 1;
                    }
                }
            }
        }
    }

    fn check_stack_overflow(&self, count: usize) {
        if self.sp + count > STACK_CAP {
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

    fn check_valid_data_address(addr: i64) {
        if i64::is_negative(addr) {
            eprintln!("[ERROR] Illegal address (negative integer not allowed)");
            exit(1);
        }
        if addr as usize >= DATA_CAP {
            eprintln!("[ERROR] Illegal data address (out of bounds)");
            exit(1);
        }
    }

    fn check_valid_inst_address(&self, addr: usize) {
        if addr as usize >= self.insts.len() {
            eprintln!("[ERROR] Illegal instruction address (out of bounds)");
            exit(1);
        }
    }

    fn write_data(&mut self, addr: usize, bytes: Vec<u8>) {
        for i in 0..bytes.len() {
            self.data[addr + i] = bytes[i];
        }
    }

    fn read_data(&self, addr: usize, size: usize) -> Vec<u8> {
        // TODO: check for out of bounds
        Vec::from(&self.data[addr..addr + size])
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

    fn collect_stack_without_zeroes(svm: Svm) -> Vec<Atom> {
        svm.stack
            .into_iter()
            .filter(|num| num > &Atom::Int(0.into()))
            .collect()
    }

    #[test]
    fn single_psh() {
        let program = vec![Inst::Push(10.into())];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 10.into());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_add() {
        let program = vec![Inst::Push(10.into()), Inst::Push(15.into()), Inst::Add];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 25.into());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_dup_top() {
        let program = vec![Inst::Push(10.into()), Inst::Push(15.into()), Inst::Dupe(0)];

        let svm = setup_and_run(program);

        assert_eq!(svm.sp, 3);
        assert_eq!(
            collect_stack_without_zeroes(svm),
            vec![Atom::Int(10), Atom::Int(15), Atom::Int(15)]
        );
    }

    #[test]
    fn single_dup_bottom() {
        let program = vec![Inst::Push(10.into()), Inst::Push(15.into()), Inst::Dupe(1)];

        let svm = setup_and_run(program);

        assert_eq!(svm.sp, 3);
        assert_eq!(
            collect_stack_without_zeroes(svm),
            vec![Atom::Int(10), Atom::Int(15), Atom::Int(10)]
        );
    }

    #[test]
    fn single_sub() {
        let program = vec![Inst::Push(15.into()), Inst::Push(10.into()), Inst::Sub];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 5.into());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_mul() {
        let program = vec![Inst::Push(2.into()), Inst::Push(10.into()), Inst::Mul];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 20.into());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_div() {
        let program = vec![Inst::Push(15.into()), Inst::Push(3.into()), Inst::Div];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 5.into());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn write_single_int_to_data() {
        let program = vec![Inst::Push(0.into()), Inst::Push(15.into()), Inst::Write];
        let svm = setup_and_run(program);
        let data_bytes: [u8; 8] = svm.data[0..8]
            .try_into()
            .expect("data slice doesn't have expected length");
        assert_eq!(i64::from_le_bytes(data_bytes), 15);
        assert_eq!(svm.sp, 0);
    }

    #[test]
    fn load_single_int_from_data() {
        let program = vec![
            Inst::Push(0.into()),
            Inst::Push(15.5.into()),
            Inst::Write,
            Inst::Push(0.into()),
            Inst::Loadf,
        ];
        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], Atom::Float(15.5.into()));
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_eq() {
        let program = vec![Inst::Push(5.into()), Inst::Push(5.into()), Inst::Eq];
        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], Atom::spawn_bool_true());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_neq() {
        let program = vec![Inst::Push(5.into()), Inst::Push(10.into()), Inst::Neq];
        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], Atom::spawn_bool_true());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_gt() {
        let program = vec![Inst::Push(10.into()), Inst::Push(5.into()), Inst::Gt];
        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], Atom::spawn_bool_true());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_lt() {
        let program = vec![Inst::Push(5.into()), Inst::Push(10.into()), Inst::Lt];
        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], Atom::spawn_bool_true());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_gte() {
        let program = vec![Inst::Push(10.into()), Inst::Push(5.into()), Inst::Gte];
        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], Atom::spawn_bool_true());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_lte() {
        let program = vec![Inst::Push(5.into()), Inst::Push(10.into()), Inst::Lte];
        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], Atom::spawn_bool_true());
        assert_eq!(svm.sp, 1);
    }
    #[test]
    fn single_jump1() {
        let program = vec![
            Inst::Push(5.into()),  // 5 sp
            Inst::Push(10.into()), // 5 10 sp
            Inst::Lte,             // 1 sp
            Inst::Jump1(5),        // sp
            Inst::Jump(0),         // ignore
            Inst::Push(5.into()),  // 5 sp
        ];
        let svm = setup_and_run(program);
        assert_eq!(svm.ip, 6);
        assert_eq!(svm.sp, 1);
    }
    #[test]
    fn single_jump0() {
        let program = vec![
            Inst::Push(10.into()),
            Inst::Push(5.into()),
            Inst::Lte,
            Inst::Jump0(5),
            Inst::Jump(0),
            Inst::Push(5.into()),
        ];
        let svm = setup_and_run(program);
        assert_eq!(svm.ip, 6);
        assert_eq!(svm.sp, 1);
    }
}
