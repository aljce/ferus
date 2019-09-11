#[macro_use]
extern crate combine;

use combine::{Parser};

use std::thread;

pub mod lexer;

pub mod expr;

use lexer::{Tokenizer};
use expr::{expn};

const STACK_SIZE: usize = 100 * 1024 * 1024;

fn run() {
    // let input = "12 + 1 mod 3 < x - 1 * 2";
    let input = "12 + 1";
    let tokenizer = Tokenizer::new(input);
    match expn().parse(tokenizer) {
        Ok((expr, _)) => println!("{}", expr),
        Err(e) => eprintln!("{}", e)
    }
}

fn main() {
    // The parser needs a lot of stack
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .unwrap();

    child.join().unwrap();
}

