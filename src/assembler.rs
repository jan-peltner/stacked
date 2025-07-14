use crate::primitives::{Atom, Inst, Program};
use std::io::Error as ioError;
use std::{collections::HashMap, path::PathBuf};

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ParseError {
    pub line: usize,
    pub token: String,
    pub message: String,
}

#[derive(Debug)]
pub enum AssemblerError {
    Io(ioError),
    InvalidFile(String),
    MissingProgSection,
    MissingMainLabel,
    InvalidLabel(ParseError, TokenKind),
    LabelRedefinition(ParseError, TokenKind),
    UndefinedInst(ParseError, TokenKind),
    MissingOperand(ParseError, TokenKind),
    UnexpectedOperand(ParseError, TokenKind),
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
                "Missing program section. Add the @prog keyword at the beginning of a line to mark the start of the program section"
            ),
            AssemblerError::InvalidFile(msg) => write!(f, "Invalid file: {}", msg),
            AssemblerError::MissingMainLabel => write!(f, "Missing main label. Add ~main symbol to mark the entry point for the program"),
            AssemblerError::InvalidLabel(parse_error, token)
            | AssemblerError::LabelRedefinition(parse_error, token)
            | AssemblerError::UndefinedInst(parse_error, token)
            | AssemblerError::MissingOperand(parse_error, token)
            | AssemblerError::UnexpectedOperand(parse_error, token) => write!(
                f,
                "Parse error on line {} for {}: {} - {}",
                parse_error.line, token, parse_error.token, parse_error.message
            ),
        }
    }
}

impl std::error::Error for AssemblerError {}

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
    let lines = asm_text.lines().map(|line| line.trim()).collect::<Vec<_>>();
    todo!()
}

pub fn serialize_program_to_bytecode(program: Program) -> Result<(), ioError> {
    todo!()
}
