use crate::primitives::{Atom, Inst, Program};
use std::fmt::Display;
use std::io::Error as ioError;
use std::{collections::HashMap, path::PathBuf};

#[derive(Debug)]
pub enum Token {
    Symbol(Sym),
    Instruction(Inst),
    Atom(Atom),
    Directive(Dir),
}

#[derive(Debug)]
pub enum Sym {
    Label,
    Var,
}

#[derive(Debug)]
pub enum Dir {
    Prog,
    Use,
    Data,
}

impl Display for Dir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Dir::Prog => write!(f, "@prog"),
            Dir::Use => write!(f, "@use"),
            Dir::Data => write!(f, "@data"),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Symbol(sym) => match sym {
                Sym::Label => write!(f, "Symbol(Label)"),
                Sym::Var => write!(f, "Symbol(Var)"),
            },
            Token::Instruction(_) => write!(f, "Instruction"),
            Token::Atom(atom) => write!(f, "Atom: {atom})"),
            Token::Directive(dir) => write!(f, "Directive({dir})"),
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
    MissingProgDirective,
    MissingMainLabel,
    InvalidLabel(ParseError, Token),
    LabelRedefinition(ParseError, Token),
    UndefinedInst(ParseError, Token),
    MissingOperand(ParseError, Token),
    UnexpectedOperand(ParseError, Token),
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
            AssemblerError::MissingProgDirective => write!(
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

fn parse_sasm(sasm_text: String) -> Result<Program, AssemblerError> {
    let mut label_instaddr_map: HashMap<String, usize> = HashMap::new();
    let mut instr_count = 0;

    let lines = sasm_text
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();

            // remove comments
            if trimmed.starts_with("#") {
                return None;
            }

            Some(trimmed)
        })
        .collect::<Vec<_>>();

    // find @prog directive and move iterator there
    if let Some(prog_line_num) = lines.iter().position(|line| line.starts_with("@prog")) {
        let prog_lines = lines.iter().skip(prog_line_num);
    } else {
        return Err(AssemblerError::MissingProgDirective);
    }

    todo!()
}

fn parse_token(token: &str) -> Token {
    todo!()
}

pub fn serialize_program_to_bytecode(program: Program) -> Result<(), ioError> {
    todo!()
}
