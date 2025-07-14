use std::path::PathBuf;
use std::str::FromStr;

use ::stacked::assembler::build_program_from_sasm;

fn main() {
    let path: PathBuf = PathBuf::from_str("./examples/fib.sasm").expect("malformed path string");
    if let Err(x) = build_program_from_sasm(path) {
        println!("{x}");
    }
}
