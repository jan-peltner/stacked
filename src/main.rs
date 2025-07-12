use stacked::svm::*;

fn main() {
    let fib: Program = vec![
        Inst::Psh(0.into()),
        Inst::Psh(1.into()),
        Inst::Dup(1), // 0 1 0
        Inst::Dup(1), // 0 1 0 1
        Inst::Add,    // 0 1 1
        Inst::Prt,
        Inst::Jmp(2),
    ];
    let mut svm = Svm::from_program(fib);
    svm.run();
}
