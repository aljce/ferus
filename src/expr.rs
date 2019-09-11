use std::fmt;

use combine::{
    Stream, satisfy, satisfy_map, choice,
};

use crate::lexer::{Literal, Operation, Token};

#[derive(Debug)]
pub enum Expr<'a> {
    Var(&'a str),
    Lit(Literal<'a>),
    Binary {
        left: Box<Expr<'a>>,
        operation: Operation,
        right: Box<Expr<'a>>
    }
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

parser!{
    pub fn expn['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        add()
    }
}

parser!{
    pub fn add['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        use Expr::*;
        let operation = |op| satisfy(move |t| t == Token::Binary(op)).map(move |_| op);
        let addition = struct_parser!{
            Binary {
                left: add().map(Box::new),
                operation: operation(Operation::Add),
                right: mult().map(Box::new)
            }
        };
        let subtraction = struct_parser!{
            Binary {
                left: add().map(Box::new),
                operation: operation(Operation::Sub),
                right: mult().map(Box::new)
            }
        };
        choice((atom(), addition, subtraction))
    }
}

parser!{
    pub fn mult['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        atom()
    }
}

parser!{
    pub fn atom['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        let variable = satisfy_map(|t| match t {
            Token::Var(name) => Some(Expr::Var(name)),
            _ => None
        });
        variable
    }
}
