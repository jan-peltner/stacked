use stacked::svm::*;

fn main() {
    let program: Vec<Inst> = vec![
        Inst::Psh(1.into()),
        Inst::Psh(10.into()),
        Inst::Add,
        Inst::Prt,
        Inst::Hlt,
    ];
    let mut svm = Svm::from_program(program);
    svm.run();
}
