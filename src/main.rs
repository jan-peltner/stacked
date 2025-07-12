use stacked::svm::*;

fn main() {
    let program: Vec<Inst> = vec![Inst::Push(1), Inst::Print, Inst::Jump(0), Inst::Halt];
    let mut svm = Svm::from_program(program);
    svm.run();
}
