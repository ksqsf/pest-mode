use std::env;

mod meta;
mod lang;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args[1].as_ref() {
        "meta_check" => meta::check(),
        "lang_check" => lang::check(&args[2]),
        "lang_analyze" => lang::analyze(&args[2]),
        // "lang_element_at_point" => lang::element_at_point(),
        mode => panic!("unknown mode {}", mode),
    }
}
