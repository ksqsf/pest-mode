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
}

pub fn analyze(rule: &str) {
    work(rule, LangMode::Analyze)
}

pub fn check(rule: &str) {
    work(rule, LangMode::Check)
}

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
