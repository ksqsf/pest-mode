//! # lang
//!
//! This mod deals with the pest-input.
//!
//! Due to Emacs limitation (the pipe is closed after sending an EOF),
//! we cannot simply read from stdin twice, so the data sent to pesta
//! should be json-encoded in the format `[<grammar>,<input>]`.

use pest::error::{ErrorVariant, InputLocation};
use pest::iterators::Pair;
use pest_vm::Vm;
use pest_meta::{parser, optimizer, validator};
use pest_meta::parser::Rule;
use std::io;
use std::io::Read;
use json::{self, JsonValue};

enum LangMode {
    Analyze,
    Check,
    ElementAtPoint(usize),
}

/// Analyze the input, given the grammar and the start rule.  Panics
/// if the grammar is incorrect.  Otherwise, the parsing result will
/// be printed to stdout.
///
/// Required Arguments:
/// 1. rule: string
///
/// Read from stdin: `(json-encode-list (list grammar input))`
pub fn analyze(rule: &str) {
    work(rule, LangMode::Analyze)
}

/// Check whether the input is correct against the given grammar and
/// the start rule.  Panics if the grammar is incorrect.  Otherwise,
/// if anything is wrong, print the errors each on a line in the
/// format `nil (<start>,<end>) <message>`.  `start` and `end` start
/// from 1.
///
/// Required Arguments:
/// 1. rule: string
///
/// Read from stdin: `(json-encode-list (list grammar input))`
pub fn check(rule: &str) {
    work(rule, LangMode::Check)
}

/// Determine the element at the current point, printing a path in a
/// format like `rule1.rule2.rule3`.  Panics if the grammar is
/// incorrect, or the arguments are invalid.
///
/// Required Arguments:
/// 1. rule: string
/// 2. pos: point position, in Emacs form `(point)` (starting from 1)
///
/// Read from stdin: `(json-encode-list (list grammar input))`
pub fn element_at_point(rule: &str, pos: &str) {
    let pos: usize = str::parse(pos).expect("pos must be a number");
    work(rule, LangMode::ElementAtPoint(pos))
}

/// The actual implementation of the functions above.
fn work(rule: &str, mode: LangMode) {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut buffer = String::new();
    handle.read_to_string(&mut buffer).expect("read stdin");;
    let json = json::parse(&buffer).expect("json");
    let (meta_buf, input_buf) = match json {
        JsonValue::Array(list) => {
            (list[0].as_str().map(|s| s.to_owned()).expect("grammar"),
             list[1].as_str().map(|s| s.to_owned()).expect("input"))
        }
        _ => panic!("malformed json input"),
    };
    let meta = parser::parse(Rule::grammar_rules, &meta_buf).expect("parse meta");
    validator::validate_pairs(meta.clone()).expect("meta pairs validation");
    let ast = parser::consume_rules(meta).expect("meta to ast");
    let vm = Vm::new(optimizer::optimize(ast.clone()));

    let result = vm.parse(rule, &input_buf);
    match mode {
        LangMode::Check => match result {
            Ok(_) => {},
            Err(error) => {
                let (beg, end) = match error.location {
                    InputLocation::Pos(pos) => (1+pos, 2+pos),
                    InputLocation::Span((beg, end)) => (1+beg, 1+end),
                };
                let message = match error.variant {
                    ErrorVariant::CustomError { message } => message.to_string(),
                    ErrorVariant::ParsingError { positives, negatives } => {
                        format!("unexpected: {:?}, expected: {:?}", negatives, positives)
                    }
                };
                println!("nil ({},{}) {}", beg, end, message);
            }
        }
        LangMode::Analyze => match result {
            Ok(pairs) => {
                let lines: Vec<_> = pairs.map(|pair| {
                    format_pair(pair, 0, true)
                }).collect();
                let lines = lines.join("\n");
                println!("{}", lines);
            }
            Err(error) => {
                println!("{}", error.renamed_rules(|r| r.to_string()));
            }
        }
        LangMode::ElementAtPoint(pos) => match result {
            Ok(mut pairs) => {
                let mut path: Vec<&str> = vec![];
                loop {
                    let pair = pairs.find(|pair| {
                        let span = pair.as_span();
                        span.start() + 1 <= pos && pos < span.end() + 1
                    });
                    match pair {
                        Some(pair) => {
                            path.push(pair.as_rule());
                            pairs = pair.into_inner();
                        }
                        None => break,
                    }
                }
                println!("{}", path.join("."));
            }
            Err(_) => {}
        },
    }
}

fn format_pair(pair: Pair<&str>, indent_level: usize, is_newline: bool) -> String {
    let indent = if is_newline {
        " ".repeat(indent_level)
    } else {
        "".to_string()
    };

    let children: Vec<_> = pair.clone().into_inner().collect();
    let len = children.len();
    let children: Vec<_> = children.into_iter().map(|pair| {
        format_pair(pair, if len > 1 { indent_level + 1 } else { indent_level }, len > 1)
    }).collect();

    let dash = if is_newline {
        "- "
    } else {
        ""
    };

    match len {
        0 => format!("{}{}{}: {:?}", indent, dash, pair.as_rule(), pair.as_span().as_str()),
        1 => format!("{}{}{} > {}", indent, dash, pair.as_rule(), children[0]),
        _ => format!("{}{}{}\n{}", indent, dash, pair.as_rule(), children.join("\n")),
    }
}
