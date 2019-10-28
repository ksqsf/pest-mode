use pest::error::{ErrorVariant, InputLocation};
use pest_vm::Vm;
use pest_meta::{parser, optimizer, validator};
use pest_meta::parser::Rule;
use std::io;
use std::io::Read;
use json::{self, JsonValue};

pub fn check(rule: &str) {
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

    match vm.parse(rule, &input_buf) {
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
}
