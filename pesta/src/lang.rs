use pest::error::{ErrorVariant, InputLocation};
use pest_vm::Vm;
use pest_meta::{parser, optimizer, validator};
use pest_meta::parser::Rule;
use std::io;
use std::io::Read;

pub fn check(rule: &str) {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut meta_buf = String::new();
    handle.read_to_string(&mut meta_buf).expect("meta read stdin");;
    let meta = parser::parse(Rule::grammar_rules, &meta_buf).expect("parse meta");
    validator::validate_pairs(meta.clone()).expect("meta pairs validation");
    let ast = parser::consume_rules(meta).expect("meta to ast");
    let vm = Vm::new(optimizer::optimize(ast.clone()));

    let mut input_buf = String::new();
    handle.read_to_string(&mut input_buf).expect("input read stdin");;
    match vm.parse(rule, &input_buf) {
        Ok(_) => {},
        Err(error) => {
            let (beg, end) = match error.location {
                InputLocation::Pos(pos) => (pos, pos),
                InputLocation::Span((beg, end)) => (beg, end),
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
}
