use std::fmt;
use combine::{
    EasyParser, ParseError, Stream, RangeStream,
    stream::{StreamOnce, Positioned, ResetStream},
    choice, attempt, eof,
    parser::char::{char, string},
    parser::range::{take_while1},
    easy::{Errors},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
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
    End,
    Fn,
    Arrow,
}

impl fmt::Display for Reserved {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Reserved::*;
        let name = match *self {
            Add => "+",
            Sub => "-",
            Mult => "*",
            Div => "div",
            Mod => "mod",
            Equal => "=",
            LessThan => "<",
            OrElse => "orelse",
            AndAlso => "andalso",
            If => "if",
            Then => "then",
            Else => "else",
            Not => "not",
            Let => "let",
            Val => "val",
            In => "in",
            End => "end",
            Fn => "fn",
            Arrow => "->",
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Token<'a> {
    Name(&'a str),
    Lit(Literal<'a>),
    Paren(Direction),
    Space(usize),
    Keyword(Reserved),
    EndOfFile,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;
        match *self {
            Name(name) => write!(f, "{}", name),
            Lit(lit) => write!(f, "{}", lit),
            Paren(Direction::Left) => write!(f, "("),
            Paren(Direction::Right) => write!(f, ")"),
            Space(c) => write!(f, "SPACE({})", c),
            Keyword(res) => write!(f, "{}", res),
            EndOfFile => write!(f, "EOF"),
        }
    }
}

parser!{
    pub fn spaces['a, Input]()(Input) -> Token<'a>
    where [ Input: RangeStream<Item = char, Range = &'a str> ]
    {
        use Token::*;
        take_while1(|c: char| c.is_whitespace()).map(|tok: &'a str| Space(tok.len()))
    }
}

parser!{
    pub fn number['a, Input]()(Input) -> Literal<'a>
    where [ Input: RangeStream<Item = char, Range = &'a str> ]
    {
        use Literal::*;
        let integer = take_while1(|c: char| c.is_digit(10)).map(|string: &'a str| string.parse::<i64>().unwrap());
        integer.map(Integer)
    }
}

const RESERVED: &'static str = " ()";

parser!{
    pub fn one['a, Input]()(Input) -> Token<'a>
    where [ Input: RangeStream<Item = char, Range = &'a str> ]
    {
        use Reserved::*;
        use Literal::*;
        use Token::*;
        let not_reserved = |c: char| RESERVED.chars().all(|r| r != c) && !c.is_digit(10);
        take_while1(not_reserved).flat_map(|tok| match tok {
            "+" => Ok(Keyword(Add)),
            "-" => Ok(Keyword(Sub)),
            "*" => Ok(Keyword(Mult)),
            "div" => Ok(Keyword(Div)),
            "mod" => Ok(Keyword(Mod)),
            "=" => Ok(Keyword(Equal)),
            "<" => Ok(Keyword(LessThan)),
            "orelse" => Ok(Keyword(OrElse)),
            "andalso" => Ok(Keyword(AndAlso)),
            "if" => Ok(Keyword(If)),
            "then" => Ok(Keyword(Then)),
            "else" => Ok(Keyword(Else)),
            "not" => Ok(Keyword(Not)),
            "let" => Ok(Keyword(Let)),
            "val" => Ok(Keyword(Val)),
            "in" => Ok(Keyword(In)),
            "end" => Ok(Keyword(End)),
            "fn" => Ok(Keyword(Fn)),
            "->" => Ok(Keyword(Arrow)),
            "true" => Ok(Lit(Boolean(true))),
            "false" => Ok(Lit(Boolean(false))),
            _ if tok.chars().all(|c: char| c.is_alphabetic()) => Ok(Name(tok)),
            _ => {
                Err(panic!("lexing failure"))
            }
        })
    }
}

parser!{
    pub fn token['a, Input]()(Input) -> Token<'a>
    where [ Input: RangeStream<Item = char, Range = &'a str> ]
    {
        use Token::*;
        use Direction::*;
        choice((
            eof().map(|_| EndOfFile),
            spaces(),
            char('(').map(|_| Paren(Left)),
            char(')').map(|_| Paren(Right)),
            number().map(Lit),
            one(),
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
        match token().easy_parse(self.stream) {
            Ok((token, rest)) => {
                println!("{:?} : {:?}", token, rest);
                self.stream = rest;
                self.current = self.size - rest.len();
                Ok(token)
            },
            Err(e) => panic!("{:?}", e)
        }
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

// #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
// pub struct Error<'a> {
//     token: &'a str,
//     message: String,
// }

// impl<'a> ParseError<Token<'a>, Token<'a>, usize> for Error<'a> {
    
// }

#[cfg(test)]
mod tests {
    use super::*;
    use Literal::*;
    use Reserved::*;
    use Token::*;

    fn run_tokenizer<'a>(mut tokenizer: Tokenizer<'a>) -> Result<Vec<Token<'a>>, combine::error::StringStreamError> {
        let mut result = vec![];
        loop {
            match tokenizer.uncons()? {
                EndOfFile => return Ok(result),
                tok => result.push(tok),
            }
        }
    }

    #[test]
    fn tokenizer_unit1() {
        let tokenizer = Tokenizer::new("1 + 2 * 31-1");
        let result = run_tokenizer(tokenizer);
        let should = vec![
            Lit(Integer(1)), Space(1), Keyword(Add), Space(1),
            Lit(Integer(2)), Space(1), Keyword(Mult), Space(1),
            Lit(Integer(31)), Keyword(Sub), Lit(Integer(1))
        ];
        assert_eq!(result, Ok(should))
    }

    #[test]
    fn tokenizer_unit2() {
        let tokenizer = Tokenizer::new("let val x = true in if x then 0 else 1 end");
        let result = run_tokenizer(tokenizer);
        let should = vec![
            Keyword(Let), Space(1), Keyword(Val), Space(1),
            Name("x"), Space(1), Keyword(Equal), Space(1),
            Lit(Boolean(true)), Space(1), Keyword(In), Space(1),
            Keyword(If), Space(1), Name("x"), Space(1),
            Keyword(Then), Space(1), Lit(Integer(0)), Space(1),
            Keyword(Else), Space(1), Lit(Integer(1)), Space(1),
            Keyword(End)
        ];
        assert_eq!(result, Ok(should))
    }

}
