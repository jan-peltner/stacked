use stacked::svm::*;

fn main() {
    let fib: Program = vec![
        Inst::Push(0.into()), // 0
        Inst::Push(1.into()), // 0 1
        Inst::Dupe(1),        // 0 1 0
        Inst::Dupe(1),        // 0 1 0 1
        Inst::Add,            // 0 1 1
        Inst::Print,
        Inst::Jump(2),
    ];
    let mut svm = Svm::from_program(fib);
    svm.run();
}
