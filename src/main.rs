#[macro_use]
extern crate combine;

use combine::{Parser};

use std::thread;
use std::fs::File;
use std::io::Write;

use rustyline::{Config, Editor, EditMode};
use rustyline::error::ReadlineError;

pub mod lexer;

pub mod expr;

use lexer::{Tokenizer};
use expr::{expn};

pub fn repl() {
    let prompt = "> ";
    let config = Config::builder()
        .edit_mode(EditMode::Emacs)
        .max_history_size(1000)
        .history_ignore_space(true)
        .build();
    let mut rl = Editor::<()>::with_config(config);
    let history_file = ".ferus_history";
    if rl.load_history(&history_file).is_err() {
        println!("no history file creating[{}]...", history_file);
        let mut file = File::create(&history_file).unwrap();
        file.write_all(b"").unwrap();
    }
    loop {
        let readline = rl.readline(prompt);
        match readline {
            Ok(ref line) if line.is_empty() => {}
            Ok(ref line) => {
                let tokenizer = Tokenizer::new(line);
                match expn().parse(tokenizer) {
                    Err(err) => {
                        eprintln!("ERROR: could not parse ({}) because {}", line, err)
                    },
                    Ok((expr, _)) => {
                        rl.add_history_entry(line);
                        println!("{}", expr.pretty());
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                eprintln!("CTRL-C");
                break
            }
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("ERROR: {}", err);
                break
            }
        }
    }
    rl.save_history(&history_file).unwrap();
}

const STACK_SIZE: usize = 100 * 1024 * 1024;

fn main() {
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(repl)
        .unwrap();
    child.join().unwrap();
}

