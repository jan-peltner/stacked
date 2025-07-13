use crate::primitives::*;
use std::{array, process::exit};

const STACK_CAP: usize = 1024;
const DATA_CAP: usize = 1024;

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

    // NOTE: the VM stack is initialized with zeroes which can be
    // problematic when testing for the expected stack size or
    // comparing against explicit zero values
    fn collect_stack_without_zeroes(svm: Svm) -> Vec<Atom> {
        svm.stack
            .into_iter()
            .filter(|num| num > &Atom::Int(0.into()))
            .collect()
    }

    use Inst::*;

    #[test]
    fn single_psh() {
        let program = vec![Push(10.into())];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 10.into());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_add() {
        let program = vec![Push(10.into()), Push(15.into()), Add];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 25.into());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_dup_top() {
        let program = vec![Push(10.into()), Push(15.into()), Dupe(0)];

        let svm = setup_and_run(program);

        assert_eq!(svm.sp, 3);
        assert_eq!(
            collect_stack_without_zeroes(svm),
            vec![10.into(), 15.into(), 15.into()]
        );
    }

    #[test]
    fn single_dup_bottom() {
        let program = vec![Push(10.into()), Push(15.into()), Dupe(1)];

        let svm = setup_and_run(program);

        assert_eq!(svm.sp, 3);
        assert_eq!(
            collect_stack_without_zeroes(svm),
            vec![10.into(), 15.into(), 10.into()]
        );
    }

    #[test]
    fn single_sub() {
        let program = vec![Push(15.into()), Push(10.into()), Sub];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 5.into());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_mul() {
        let program = vec![Push(2.into()), Push(10.into()), Mul];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 20.into());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_div() {
        let program = vec![Push(15.into()), Push(3.into()), Div];

        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], 5.into());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn write_single_int_to_data() {
        let program = vec![Push(0.into()), Push(15.into()), Write];
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
            Push(0.into()),
            Push(15.5.into()),
            Write,
            Push(0.into()),
            Loadf,
        ];
        let svm = setup_and_run(program);
        assert_eq!(svm.stack[0], Atom::Float(15.5.into()));
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_eq() {
        let program = vec![Push(5.into()), Push(5.into()), Eq];
        let svm = setup_and_run(program);
        assert!(svm.stack[0].is_true());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_neq() {
        let program = vec![Push(5.into()), Push(10.into()), Neq];
        let svm = setup_and_run(program);
        assert!(svm.stack[0].is_true());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_gt() {
        let program = vec![Push(10.into()), Push(5.into()), Gt];
        let svm = setup_and_run(program);
        assert!(svm.stack[0].is_true());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_lt() {
        let program = vec![Push(5.into()), Push(10.into()), Lt];
        let svm = setup_and_run(program);
        assert!(svm.stack[0].is_true());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_gte() {
        let program = vec![Push(10.into()), Push(5.into()), Gte];
        let svm = setup_and_run(program);
        assert!(svm.stack[0].is_true());
        assert_eq!(svm.sp, 1);
    }

    #[test]
    fn single_lte() {
        let program = vec![Push(5.into()), Push(10.into()), Lte];
        let svm = setup_and_run(program);
        assert!(svm.stack[0].is_true());
        assert_eq!(svm.sp, 1);
    }
    #[test]
    fn single_jump1() {
        let program = vec![
            Push(5.into()),  // 5 sp
            Push(10.into()), // 5 10 sp
            Lte,             // 1 sp
            Jump1(5),        // sp
            Jump(0),         // <skip>
            Push(5.into()),  // 5 sp
        ];
        let svm = setup_and_run(program);
        assert_eq!(svm.ip, 6);
        assert_eq!(svm.sp, 1);
    }
    #[test]
    fn single_jump0() {
        let program = vec![
            Push(10.into()),
            Push(5.into()),
            Lte,
            Jump0(5),
            Jump(0),
            Push(5.into()),
        ];
        let svm = setup_and_run(program);
        assert_eq!(svm.ip, 6);
        assert_eq!(svm.sp, 1);
    }
}
