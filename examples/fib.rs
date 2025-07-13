use stacked::primitives::*;
use stacked::svm::*;

fn main() {
    let fib_n: Program = vec![
        Inst::Push(0.into()),
        Inst::Push(1.into()),
        Inst::Push(0.into()),
        Inst::Push(8.into()),
        Inst::Write,
        Inst::Push(0.into()),
        Inst::Loadi,
        Inst::Push(0.into()),
        Inst::Lte,
        Inst::Jump1(20),
        Inst::Push(0.into()),
        Inst::Push(0.into()),
        Inst::Loadi,
        Inst::Push(1.into()),
        Inst::Sub,
        Inst::Write,
        Inst::Dupe(1),
        Inst::Dupe(1),
        Inst::Add,
        Inst::Jump(5),
        Inst::Print,
    ];
    let mut svm = Svm::from_program(fib_n);
    svm.run();
}
