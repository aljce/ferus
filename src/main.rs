#[macro_use]
extern crate combine;

use docopt::Docopt;
use serde::Deserialize;

use combine::*;

use std::path::PathBuf;
use std::fs::File;
use std::io::Read;
use std::io::Write;

use rustyline::{Config, Editor, EditMode};
use rustyline::error::ReadlineError;

pub mod lexer;
pub mod expr;

use lexer::{Tokenizer};
use expr::{prog};

const USAGE: &'static str = "
[ferus] an ocaml clone

Usage:
  ferus [options]
  ferus [options] <source>

Options:
   -h, --help  Display this help message
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_source: Option<PathBuf>
}

pub fn interpret<'a>(source: &'a str) {
    let tokenizer = Tokenizer::new(source);
    match prog().easy_parse(tokenizer) {
        Err(err) => {
            eprintln!("ERROR: could not parse ({}) because {}", source, err)
        },
        Ok((expr, _)) => {
            match expr.eval() {
                Ok(value) => println!("{}", value),
                Err(err) => eprintln!("{:?}", err),
            }
        }
    }
}

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
                rl.add_history_entry(line);
                interpret(line)
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

pub fn file(source: PathBuf) {
    match File::open(&source) {
        Err(err) => eprintln!("Could not open file {:?} because: {}", source, err),
        Ok(mut file) => {
            let mut buf = String::new();
            match file.read_to_string(&mut buf) {
                Err(err) => eprintln!("Could not read source file {:?} because: {}", source, err),
                Ok(_) => interpret(&buf),
            }
        }
    }
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());
    match args.arg_source {
        None => repl(),
        Some(source) => file(source),
    }
}

