#![feature(uniform_paths)]

#[macro_use]
extern crate failure;
#[macro_use]
extern crate lazy_static;
extern crate clap;
extern crate inkwell;
extern crate libc;
extern crate linked_hash_map;

use std::fs::File;
use std::io::prelude::*;

use clap::{App, Arg};

mod lexer;
use lexer::lexer::*;

mod parser;
use parser::parser::*;

mod types;
use types::walker::*;

const INPUT_FILE: &'static str = "input_file";
const OUTPUT_FILE: &'static str = "output_file";

fn read_file(file_name: &str) -> Result<String, String> {
    if let Ok(mut file) = File::open(file_name) {
        let mut contents = String::new();
        let _ = file
            .read_to_string(&mut contents)
            .map_err(|err| format!("{}", err))?;
        Ok(contents)
    } else {
        Err(format!("failed to open: {}", file_name))
    }
}

fn main() {
    let matches = App::new("rr-llvm-ir")
        .version("1.0")
        .author("rchaser53. <tayoshizawa29@gmail.com>")
        .arg(Arg::with_name(INPUT_FILE).index(1))
        .arg(Arg::with_name(OUTPUT_FILE).index(2))
        .get_matches();

    let input_file_name = matches.value_of(INPUT_FILE).unwrap_or("input.txt");
    // let output_file_name = matches.value_of(OUTPUT_FILE).unwrap_or("output.ll");
    match read_file(input_file_name) {
        Ok(input) => {
            let mut lexer = Lexer::new(&input);
            let mut parser = Parser::new(&mut lexer);

            let statements = parser.parse_program();
            if parser.has_error() {
                panic!("{}", parser.emit_error());
            }
            let mut walker = Walker::new();
            walker.walk(statements);
        }
        Err(error) => {
            panic!("{}", error);
        }
    };
}
