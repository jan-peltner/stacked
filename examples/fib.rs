use stacked::primitives::{Inst, Program};
use stacked::svm::Svm;

fn main() {
    use Inst::*;

    let fib_n: Program = vec![
        Push(0.into()),
        Push(1.into()),
        Push(0.into()),
        Push(8.into()),
        Write,
        Push(0.into()),
        Loadi,
        Push(0.into()),
        Lte,
        Jump1(20),
        Push(0.into()),
        Push(0.into()),
        Loadi,
        Push(1.into()),
        Sub,
        Write,
        Dupe(1),
        Dupe(1),
        Add,
        Jump(5),
        Print,
    ];
    let mut svm = Svm::from_program(fib_n);
    svm.run();
}
