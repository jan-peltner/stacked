use crate::primitives::{Dir, NumLit, Op, Program, Sym, Token};
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

    UndefinedToken(ParseError),

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
            | AssemblerError::UndefinedToken(parse_error)
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
        let tokens = tokenize(file_str_contents)?;
        todo!()
    } else {
        return Err(AssemblerError::InvalidFile(
            "Path is not a valid UTF-8 string".to_string(),
        ));
    }
}

fn tokenize(sasm_text: String) -> Result<Program, AssemblerError> {
    let mut label_instaddr_map: HashMap<Sym, usize> = HashMap::new();
    let mut instr_count = 0;
    let mut found_main_label = false;
    let lines = sasm_text
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();

            // remove comments and empty lines
            if trimmed.starts_with("#") || trimmed.is_empty() {
                return None;
            }

            Some(trimmed)
        })
        .collect::<Vec<_>>();
    dbg!(&lines);
    // first pass: find labels and map them to instruction address space
    // also checks for @prog directive and ~main label to validate the sasm
    if let Some(prog_line_num) = lines.iter().position(|line| line.starts_with("@prog")) {
        let it = lines.iter().skip(prog_line_num + 1);
        for line in it {
            match parse_token(line)? {
                (Token::Symbol(sym), _) => match sym {
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

    let mut tokens: Vec<Token> = Vec::new();

    // second pass
    'outer: for line in lines {
        let mut idx = 0;
        while idx < line.len() {
            let char = line[idx..].chars().next().unwrap();
            if char == '#' {
                continue 'outer;
            }
            if char.is_whitespace() {
                idx += 1;
                continue;
            }
            let (token, token_len) = parse_token(&line[idx..])?;
            idx += token_len;
            tokens.push(token);
            dbg!(&tokens);
        }
    }
    todo!()
}

fn parse_token(line: &str) -> Result<(Token, usize), AssemblerError> {
    let token = get_token_slice(line);
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
            return Ok((Token::Symbol(Sym::Label(token.to_string())), token.len()));
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
                return Ok((Token::Directive(dir), token.len()));
            }

            Err(AssemblerError::UndefinedDirective(ParseError {
                parsed: token.to_string(),
                message: format!("{} is not a valid directive.", token),
            }))
        }
        // opcode or literal
        Some(_) => {
            if let Some(op) = Op::from_str(token) {
                return Ok((Token::Opcode(op), token.len()));
            }
            if let Some(num_lit) = NumLit::from_str(token) {
                return Ok((Token::NumericLiteral(num_lit), token.len()));
            }

            Err(AssemblerError::UndefinedToken(ParseError {
                parsed: token.to_string(),
                message: format!("{} is not a valid opcode or literal", token),
            }))
        }
        None => unreachable!("{token}"),
    }
}

fn get_token_slice(line: &str) -> &str {
    let token_end_index = line.find(|c: char| c.is_whitespace()).unwrap_or(line.len());
    &line[0..token_end_index]
}

pub fn serialize_program_to_bytecode(_program: Program) -> Result<(), ioError> {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::primitives::{Dir, NumLit, Sym, Token};

    #[test]
    fn parse_invalid_opcode() {
        assert!(matches!(
            parse_token("InvalidInst"),
            Err(AssemblerError::UndefinedToken(_))
        ));
    }

    #[test]
    fn parse_invalid_num_lit() {
        assert!(matches!(
            parse_token("shq12"),
            Err(AssemblerError::UndefinedToken(_))
        ));
    }

    #[test]
    fn parse_label() {
        match parse_token("~main") {
            Ok((Token::Symbol(Sym::Label(label)), _)) => {
                assert_eq!(label, "~main");
            }
            _ => panic!("Expected label token"),
        }
    }

    #[test]
    fn parse_directives() {
        assert!(matches!(
            parse_token("@prog"),
            Ok((Token::Directive(Dir::Prog), _))
        ));
        assert!(matches!(
            parse_token("@use"),
            Ok((Token::Directive(Dir::Use), _))
        ));
        assert!(matches!(
            parse_token("@data"),
            Ok((Token::Directive(Dir::Data), _))
        ));
    }

    #[test]
    fn parse_integer_literals() {
        // positive integers
        assert!(matches!(
            parse_token("42"),
            Ok((Token::NumericLiteral(NumLit::Int(42)), _))
        ));
        assert!(matches!(
            parse_token("0"),
            Ok((Token::NumericLiteral(NumLit::Int(0)), _))
        ));
        assert!(matches!(
            parse_token("1234567890"),
            Ok((Token::NumericLiteral(NumLit::Int(1234567890)), _))
        ));

        // negative integers
        assert!(matches!(
            parse_token("-42"),
            Ok((Token::NumericLiteral(NumLit::Int(-42)), _))
        ));
        assert!(matches!(
            parse_token("-1"),
            Ok((Token::NumericLiteral(NumLit::Int(-1)), _))
        ));

        // edge cases for i64
        assert!(matches!(
            parse_token("9223372036854775807"), // i64::MAX
            Ok((Token::NumericLiteral(NumLit::Int(9223372036854775807)), _))
        ));
        assert!(matches!(
            parse_token("-9223372036854775808"), // i64::MIN
            Ok((Token::NumericLiteral(NumLit::Int(-9223372036854775808)), _))
        ));
    }

    #[test]
    fn parse_float_literals() {
        // basic floats
        assert!(matches!(
            parse_token("3.14"),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if (f - 3.14).abs() < f64::EPSILON
        ));
        assert!(matches!(
            parse_token("0.0"),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if f == 0.0
        ));
        assert!(matches!(
            parse_token("-2.5"),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if (f - (-2.5)).abs() < f64::EPSILON
        ));

        // scientific notation
        assert!(matches!(
            parse_token("1e10"),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if (f - 1e10).abs() < f64::EPSILON
        ));
        assert!(matches!(
            parse_token("2.5e-3"),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if (f - 2.5e-3).abs() < f64::EPSILON
        ));
        assert!(matches!(
            parse_token("-1.23E+4"),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if (f - (-1.23E+4)).abs() < f64::EPSILON
        ));

        // special float values
        assert!(matches!(
            parse_token("inf"),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if f.is_infinite() && f.is_sign_positive()
        ));
        assert!(matches!(
            parse_token("-inf"),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if f.is_infinite() && f.is_sign_negative()
        ));
        assert!(matches!(
            parse_token("NaN"),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if f.is_nan()
        ));
    }

    #[test]
    fn parse_numeric_edge_cases() {
        // numbers that are too large for i64 should parse as float
        assert!(matches!(
            parse_token("18446744073709551616"), // u64::MAX + 1, too large for i64
            Ok((Token::NumericLiteral(NumLit::Float(_)), _))
        ));

        // decimal point with no fractional part should still be float
        assert!(matches!(
            parse_token("42."),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if (f - 42.0).abs() < f64::EPSILON
        ));

        // leading decimal point
        assert!(matches!(
            parse_token(".5"),
            Ok((Token::NumericLiteral(NumLit::Float(f)), _)) if (f - 0.5).abs() < f64::EPSILON
        ));
    }
}
