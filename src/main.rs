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
    let input = "let val x = 2 in if 12 + (1 * 2) mod 3 < x - 2 ** 3 then 1 else 0 end";
    // use combine::StreamOnce;
    // let mut tokenizer = Tokenizer::new(input);
    // while let Ok(t) = tokenizer.uncons() {
    //     println!("{:?}", t)
    // };
    let tokenizer = Tokenizer::new(input);
    match expn().parse(tokenizer) {
        Ok((expr, _)) => println!("{}", expr),
        Err(e) => eprintln!("{}", e)
    }
}

fn main() {
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .unwrap();

    child.join().unwrap();
}

