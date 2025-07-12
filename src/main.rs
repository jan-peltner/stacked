use stacked::svm::*;

fn main() {
    let fib: Program = vec![
        Inst::Psh(0.into()),
        Inst::Psh(1.into()),
        Inst::Adk,
        Inst::Prt,
        Inst::Jmp(2),
    ];
    let mut svm = Svm::from_program(fib);
    svm.run();
}
