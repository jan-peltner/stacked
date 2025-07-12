use std::process::exit;

const STACK_SIZE: usize = 10;

#[derive(Debug)]
pub enum Inst {
    Push(i64),
    Add,
    Sub,
    Print, // prints the stack
    Jump(usize),
    Halt,
}

#[derive(Debug)]
pub struct Svm {
    pub ip: usize,
    pub sp: usize,
    pub stack: [i64; STACK_SIZE],
    pub insts: Vec<Inst>,
}

impl Svm {
    pub fn from_program(program: Vec<Inst>) -> Self {
        Svm {
            ip: 0,
            sp: 0,
            stack: [0; STACK_SIZE],
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
