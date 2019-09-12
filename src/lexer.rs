use std::fmt;
use combine::{
    Parser, Stream, RangeStream,
    stream::{StreamOnce, Positioned, ResetStream},
    choice, between, attempt,
    parser::char::{char, spaces, string},
    parser::range::{take_while1},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Literal<'a> {
    Integer(i64),
    String(&'a str),
    Boolean(bool),
}

impl<'a> fmt::Display for Literal<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Literal::*;
        match *self {
            Integer(i) => write!(f, "{}", i),
            String(s)  => write!(f, "{}", s),
            Boolean(b) => write!(f, "{}", b)
        }
    }
}
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum Reserved {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Exp,
    Equal,
    LessThan,
    OrElse,
    AndAlso,
    If,
    Then,
    Else,
    Not,
    Let,
    Val,
    In,
    End
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Token<'a> {
    Name(&'a str),
    Lit(Literal<'a>),
    Paren(Direction),
    Keyword(Reserved),
}

parser!{
    pub fn literal['a, Input]()(Input) -> Literal<'a>
    where [ Input: RangeStream<Item = char, Range = &'a str> ]
    {
        use Literal::*;
        let integer = take_while1(|c: char| c.is_digit(10)).map(|string: &'a str| string.parse::<i64>().unwrap());
        let boolean = choice((
            attempt(string("true")).map(|_| true),
            attempt(string("false")).map(|_| false)
        ));
        choice((integer.map(Integer), boolean.map(Boolean)))
    }
}

parser!{
    pub fn reserved[Input]()(Input) -> Reserved
    where [ Input: Stream<Item = char> ]
    {
        use Reserved::*;
        choice((
            attempt(char('+')).map(|_| Add),
            attempt(char('-')).map(|_| Sub),
            attempt(string("**")).map(|_| Exp),
            attempt(char('*')).map(|_| Mult),
            attempt(string("div")).map(|_| Div),
            attempt(string("mod")).map(|_| Mod),
            attempt(char('=')).map(|_| Equal),
            attempt(char('<')).map(|_| LessThan),
            attempt(string("orelse")).map(|_| OrElse),
            attempt(string("andalso")).map(|_| AndAlso),
            attempt(string("if")).map(|_| If),
            attempt(string("then")).map(|_| Then),
            attempt(string("else")).map(|_| Else),
            attempt(string("not")).map(|_| Not),
            attempt(string("let")).map(|_| Let),
            attempt(string("val")).map(|_| Val),
            attempt(string("in")).map(|_| In),
            attempt(string("end")).map(|_| End)
        ))
    }
}

parser!{
    pub fn token['a, Input]()(Input) -> Token<'a>
    where [ Input: RangeStream<Item = char, Range = &'a str> ]
    {
        use Token::*;
        use Direction::*;
        choice((
            literal().map(Lit),
            char('(').map(|_| Paren(Left)),
            char(')').map(|_| Paren(Right)),
            reserved().map(Keyword),
            take_while1(|c: char| c.is_alphabetic()).map(Name)
        ))
    }
}

pub struct Tokenizer<'a> {
    stream: &'a str,
    size: usize,
    current: usize
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct Checkpoint<'a> {
    stream: &'a str
}

impl<'a> Tokenizer<'a> {
    pub fn new(stream: &'a str) -> Tokenizer<'a> {
        Tokenizer { stream, size: stream.len(), current: 0 }
    }
}

impl<'a> StreamOnce for Tokenizer<'a> {
    type Item = Token<'a>;
    type Range = Token<'a>;
    type Position = usize;
    type Error = combine::error::StringStreamError;
    fn uncons(&mut self) -> Result<Token<'a>, Self::Error> {
        let mut lex_token = between(spaces(), spaces(), token());
        lex_token.parse(self.stream).map(|(token, rest)| {
            self.stream = rest;
            self.current = self.size - rest.len();
            token
        })
    }
}

impl<'a> Positioned for Tokenizer<'a> {
    fn position(&self) -> usize {
        self.current
    }
}

impl<'a> ResetStream for Tokenizer<'a> {
    type Checkpoint = Checkpoint<'a>;
    fn checkpoint(&self) -> Checkpoint<'a> {
        Checkpoint { stream: self.stream }
    }
    fn reset(&mut self, checkpoint: Checkpoint<'a>) -> Result<(), Self::Error> {
        self.stream = checkpoint.stream;
        Ok(())
    }
}
