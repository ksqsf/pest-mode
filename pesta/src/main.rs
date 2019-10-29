//! # Pesta
//!
//! Pesta stands for "Pest Analyzer".  It uses pest_vm to generate a
//! parser from the grammar, and then parses the input.
use std::env;

mod meta;
mod lang;

/// Currently, there are 4 commands.
///
/// + `meta_check`: Check whether the grammar itself is correct.  This supports flymake in pest-mode.
/// + `lang_check`: Check whether the input is correct.  This supports flymake in pest-input-mode.
/// + `lang_analyze`: Analyze the input and generate a pest.rs-style report.  This supports `pest-analyze-input`.
/// + `lang_point`: Determine the element under the point.  This supports eldoc in pest-input-mode.
///
/// For details, read the documentation of the corresponding functions.
fn main() {
    let args: Vec<String> = env::args().collect();
    match args[1].as_ref() {
        "meta_check" => meta::check(),
        "lang_check" => lang::check(&args[2]),
        "lang_analyze" => lang::analyze(&args[2]),
        "lang_point" => lang::element_at_point(&args[2], &args[3]),
        mode => panic!("unknown mode {}", mode),
    }
}
