use stacked::svm::*;

fn main() {
    let program: Vec<Inst> = vec![
        Inst::Push(1.into()),
        Inst::Push(10.into()),
        Inst::Add,
        Inst::Print,
        Inst::Halt,
    ];
    let mut svm = Svm::from_program(program);
    svm.run();
}
