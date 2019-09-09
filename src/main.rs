extern crate combine;

use combine::Parser;

pub mod lexer;

fn main() {
    let example = lexer::tokens().parse("12 + 1 mod 3 < x - 1 * 2");
    println!("{:?}", example);
}

