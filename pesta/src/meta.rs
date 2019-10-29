//! # meta
//!
//! This mod deals with the grammar.

use pest::error::{Error, ErrorVariant, InputLocation};
use pest_meta::parser::{self, Rule};
use pest_meta::validator;
use std::io;
use std::io::Read;

/// Check the meta language.
///
/// + Required arguments: None.
/// + Read from stdin: verbatim grammar. (Not json-encoded.)
pub fn check() {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut meta_unparsed = String::new();
    handle.read_to_string(&mut meta_unparsed).expect("cannot read stdin");
    let meta_parsed = parser::parse(Rule::grammar_rules, &meta_unparsed).map_err(|error| {
        error.renamed_rules(|rule| match *rule {
            Rule::grammar_rule => "rule".to_owned(),
            Rule::_push => "push".to_owned(),
            Rule::assignment_operator => "`=`".to_owned(),
            Rule::silent_modifier => "`_`".to_owned(),
            Rule::atomic_modifier => "`@`".to_owned(),
            Rule::compound_atomic_modifier => "`$`".to_owned(),
            Rule::non_atomic_modifier => "`!`".to_owned(),
            Rule::opening_brace => "`{`".to_owned(),
            Rule::closing_brace => "`}`".to_owned(),
            Rule::opening_paren => "`(`".to_owned(),
            Rule::positive_predicate_operator => "`&`".to_owned(),
            Rule::negative_predicate_operator => "`!`".to_owned(),
            Rule::sequence_operator => "`&`".to_owned(),
            Rule::choice_operator => "`|`".to_owned(),
            Rule::optional_operator => "`?`".to_owned(),
            Rule::repeat_operator => "`*`".to_owned(),
            Rule::repeat_once_operator => "`+`".to_owned(),
            Rule::comma => "`,`".to_owned(),
            Rule::closing_paren => "`)`".to_owned(),
            Rule::quote => "`\"`".to_owned(),
            Rule::insensitive_string => "`^`".to_owned(),
            Rule::range_operator => "`..`".to_owned(),
            Rule::single_quote => "`'`".to_owned(),
            other_rule => format!("{:?}", other_rule)
        })
    })
        .map_err(|e| vec![e])
        .and_then(|pairs| validator::validate_pairs(pairs.clone()));
    report_meta(meta_parsed);
}

/// Report either "success" (t) or "error" (nil) based on parse results.
///
/// Error Format: `[BOL]nil (<start>,<end>) <message>[EOL]`. `start`
/// and `end` start from 1.
fn report_meta(result: Result<Vec<&str>, Vec<Error<Rule>>>) {
    match result {
        Ok(_) => println!("t"),
        Err(errors) => errors.iter().for_each(|error| {
            let (beg, end) = match &error.location {
                InputLocation::Pos(pos) => (1+*pos, 2+*pos),
                InputLocation::Span((beg, end)) => (1+*beg, 1+*end),
            };
            let message = match &error.variant {
                ErrorVariant::CustomError { message } => message,
                _ => unreachable!(),
            };
            println!("nil ({},{}) {}", beg, end, message);
        })
    }
}
