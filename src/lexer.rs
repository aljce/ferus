use combine::{
    Parser, ParseError,
    Stream, RangeStream,
    stream::{StreamOnce, Positioned, ResetStream},
    choice, unexpected_any, between,
    parser::char::{char, spaces, string},
    parser::range::{take_while1},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Literal<'a> {
    Integer(i64),
    String(&'a str),
    Boolean(bool)
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum Direction {
    Left,
    Right
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum Operation {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Equal,
    LessThan
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum Reserved {} /// no reserved keywords yet

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Token<'a> {
    Var(&'a str),
    Lit(Literal<'a>),
    Paren(Direction),
    Binary(Operation),
    Keyword(Reserved)
}

fn literal<'a, Input>() -> impl Parser<Input, Output = Literal<'a>>
where Input: RangeStream<Item = char, Range = &'a str>,
      Input::Error: ParseError<Input::Item, Input::Range, Input::Position>
{
    let integer = take_while1(|c: char| c.is_digit(10)).map(|string: &'a str| string.parse::<i64>().unwrap());
    let boolean = choice((string("true").map(|_| true), string("false").map(|_| false)));
    choice((integer.map(Literal::Integer), boolean.map(Literal::Boolean)))
}

fn operation<Input>() -> impl Parser<Input, Output = Operation>
where Input: Stream<Item = char>,
      Input::Error: ParseError<Input::Item, Input::Range, Input::Position>
{
    choice((
        char('+').map(|_| Operation::Add),
        char('-').map(|_| Operation::Sub),
        char('*').map(|_| Operation::Mult),
        string("div").map(|_| Operation::Div),
        string("mod").map(|_| Operation::Mod),
        char('=').map(|_| Operation::Equal),
        char('<').map(|_| Operation::LessThan)
    ))
}

fn reserved<Input>() -> impl Parser<Input, Output = Reserved>
where Input: Stream<Item = char>,
      Input::Error: ParseError<Input::Item, Input::Range, Input::Position>
{
    unexpected_any("No reserved keywords yet")
}

pub fn token<'a, Input>() -> impl Parser<Input, Output = Token<'a>>
where Input: RangeStream<Item = char, Range = &'a str>,
      Input::Error: ParseError<Input::Item, Input::Range, Input::Position>
{
    let paren = choice((
        char('(').map(|_| Token::Paren(Direction::Left)),
        char(')').map(|_| Token::Paren(Direction::Right))
    ));
    choice((
        literal().map(Token::Lit),
        paren,
        operation().map(Token::Binary),
        reserved().map(Token::Keyword),
        take_while1(|c: char| c.is_alphabetic()).map(Token::Var)
    ))
}

pub struct Tokenizer<'a> {
    stream: &'a str,
    size: usize,
    current: usize
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
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
