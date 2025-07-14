use crate::primitives::{Atom, Inst, Program};
use std::io::Error as ioError;
use std::{collections::HashMap, path::PathBuf};

pub enum TokenKind {
    Symbol,
    Inst,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Symbol => write!(f, "Symbol"),
            TokenKind::Inst => write!(f, "Instruction"),
        }
    }
}

pub struct ParseError {
    pub kind: TokenKind,
    pub line: usize,
    pub token: String,
    pub message: String,
}

pub enum AssemblerError {
    Io(ioError),
    InvalidFile(String),
    MissingProgSection,
    InvalidLabel(ParseError),
    LabelRedefinition(ParseError),
    UndefinedInst(ParseError),
    MissingOperand(ParseError),
    UnexpectedOperand(ParseError),
}

impl From<ioError> for AssemblerError {
    fn from(value: ioError) -> Self {
        Self::Io(value)
    }
}

impl std::fmt::Display for AssemblerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssemblerError::Io(error) => write!(f, "Error reading file: {}", error),
            AssemblerError::MissingProgSection => write!(
                f,
                "Missing program section. Add the @prog label to mark the beginning of the program"
            ),
            AssemblerError::InvalidFile(msg) => write!(f, "Invalid file: {}", msg),
            AssemblerError::InvalidLabel(parse_error)
            | AssemblerError::LabelRedefinition(parse_error)
            | AssemblerError::UndefinedInst(parse_error)
            | AssemblerError::MissingOperand(parse_error)
            | AssemblerError::UnexpectedOperand(parse_error) => write!(
                f,
                "Parse error on line {} for {}: {} - {}",
                parse_error.line, parse_error.kind, parse_error.token, parse_error.message
            ),
        }
    }
}

pub fn build_program_from_sasm(path: PathBuf) -> Result<Program, AssemblerError> {
    if let Some(file_name) = path.to_str() {
        if !file_name.ends_with(".sasm") {
            return Err(AssemblerError::InvalidFile(format!(
                "File {} is not a valid .sasm file",
                file_name
            )));
        }
        let file_str_contents =
            std::fs::read_to_string(&path).map_err(|e| AssemblerError::Io(e))?;
        return parse_sasm(file_str_contents);
    } else {
        return Err(AssemblerError::InvalidFile(
            "Path is not a valid UTF-8 string".to_string(),
        ));
    }
}

fn parse_sasm(asm_text: String) -> Result<Program, AssemblerError> {
    todo!()
}

pub fn serialize_program_to_bytecode(program: Program) -> Result<(), ioError> {
    todo!()
}
