use crate::primitives::{Dir, Program, Sym, Token};
use std::io::Error as ioError;
use std::{collections::HashMap, path::PathBuf};

#[derive(Debug)]
pub struct ParseError {
    pub parsed: String,
    pub message: String,
}

#[derive(Debug)]
pub enum AssemblerError {
    Io(ioError),
    InvalidFile(String),

    MissingProgDirective,
    InvalidDirective(ParseError),
    UndefinedDirective(ParseError),

    MissingMainLabel,
    InvalidLabel(ParseError),
    LabelRedefinition(ParseError),

    UndefinedInst(ParseError),

    MissingLiteral(ParseError),
    UnexpectedLiteral(ParseError),
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
                "Missing program directive. Add the @prog keyword at the beginning of a line to mark the start of the program section"
            ),
            AssemblerError::InvalidFile(msg) => write!(f, "Invalid file: {}", msg),
            AssemblerError::MissingMainLabel => write!(f, "Missing main label. Add ~main symbol after @prog directive to mark the entry point for the program"),
            AssemblerError::InvalidLabel(parse_error)
            | AssemblerError::InvalidDirective(parse_error)
            | AssemblerError::UndefinedDirective(parse_error)
            | AssemblerError::LabelRedefinition(parse_error)
            | AssemblerError::UndefinedInst(parse_error)
            | AssemblerError::MissingLiteral(parse_error)
            | AssemblerError::UnexpectedLiteral(parse_error) => write!(
                f,
                "Parse error for {}: {}",
                parse_error.parsed, parse_error.message
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
    let mut label_instaddr_map: HashMap<Sym, usize> = HashMap::new();
    let mut instr_count = 0;
    let mut found_main_label = false;
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

    // first pass: find labels and map them to instruction address space
    // also checks for @prog directive and ~main label to validate the sasm
    if let Some(prog_line_num) = lines.iter().position(|line| line.starts_with("@prog")) {
        let it = lines.iter().skip(prog_line_num + 1);
        for line in it {
            match parse_token(line)? {
                Token::Symbol(sym) => match sym {
                    Sym::Label(label_name) => {
                        if label_name == "~main" {
                            found_main_label = true
                        }
                        label_instaddr_map.insert(Sym::Label(label_name), instr_count);
                    }
                    Sym::Var(_) => todo!(),
                },
                _ => {
                    instr_count += 1;
                }
            };
        }

        if !found_main_label {
            return Err(AssemblerError::MissingMainLabel);
        }
    } else {
        return Err(AssemblerError::MissingProgDirective);
    }

    todo!()
}

fn parse_token(line: &str) -> Result<Token, AssemblerError> {
    let token = build_token(line);
    match token.chars().next() {
        // label
        Some('~') => {
            if !token
                .chars()
                .skip(1)
                .all(|c: char| c.is_ascii_alphanumeric() || c == '_')
            {
                return Err(AssemblerError::InvalidLabel(ParseError {
                    parsed: token.to_string(),
                    message: "Only alphanumeric ascii characters and '_' are allowed for symbols"
                        .to_string(),
                }));
            }
            return Ok(Token::Symbol(Sym::Label(token.to_string())));
        }
        // directive
        Some('@') => {
            if !token
                .chars()
                .skip(1)
                .all(|c: char| c.is_ascii_alphanumeric() || c == '_')
            {
                return Err(AssemblerError::InvalidDirective(ParseError {
                    parsed: token.to_string(),
                    message: "Only alphanumeric ascii characters and '_' are allowed for symbols"
                        .to_string(),
                }));
            }

            if let Some(dir) = Dir::from_str(token) {
                return Ok(Token::Directive(dir));
            }

            Err(AssemblerError::UndefinedDirective(ParseError {
                parsed: token.to_string(),
                message: format!("{} is not a valid directive.", token),
            }))
        }
        Some(ch) => {
            todo!()
        }
        None => unreachable!(),
    }
}

fn build_token(line: &str) -> &str {
    let token_end_index = line.find(|c: char| c.is_whitespace()).unwrap_or(line.len());
    &line[0..token_end_index]
}

pub fn serialize_program_to_bytecode(program: Program) -> Result<(), ioError> {
    todo!()
}
