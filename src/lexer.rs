use std::fmt;
use combine::{
    EasyParser, Stream, RangeStream,
    stream::{StreamOnce, Positioned, ResetStream},
    choice, eof, satisfy_map,
    parser::range::{take_while1},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum Literal<'a> {
    Integer(i64),
    Boolean(bool),
    String(&'a str),
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
pub enum Delimiter {
    Paren(Direction),
    Semicolon,
    Comma,
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Direction::*;
        use Delimiter::*;
        let name = match *self {
            Paren(Left) => "(",
            Paren(Right) => ")",
            Semicolon => ";",
            Comma => ",",
        };
        write!(f, "{}", name)
    }
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
            Arrow => "=>",
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Token<'a> {
    Name(&'a str),
    Lit(Literal<'a>),
    Space(usize),
    Delim(Delimiter),
    Keyword(Reserved),
    EndOfFile,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;
        match *self {
            Name(name) => write!(f, "{}", name),
            Lit(lit) => write!(f, "{}", lit),
            Space(c) => write!(f, "SPACE({})", c),
            Delim(d) => write!(f, "{}", d),
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

parser!{
    pub fn alphabetic['a, Input]()(Input) -> Token<'a>
    where [ Input: RangeStream<Item = char, Range = &'a str> ]
    {
        use Reserved::*;
        use Literal::*;
        use Token::*;
        take_while1(|c: char| c.is_alphabetic()).map(|tok| match tok {
            "div" => Keyword(Div),
            "mod" => Keyword(Mod),
            "orelse" => Keyword(OrElse),
            "andalso" => Keyword(AndAlso),
            "if" => Keyword(If),
            "then" => Keyword(Then),
            "else" => Keyword(Else),
            "not" => Keyword(Not),
            "let" => Keyword(Let),
            "val" => Keyword(Val),
            "in" => Keyword(In),
            "end" => Keyword(End),
            "fn" => Keyword(Fn),
            "true" => Lit(Boolean(true)),
            "false" => Lit(Boolean(false)),
            _ => Name(tok)
        })
    }
}

parser!{
    pub fn delimiter[Input]()(Input) -> Delimiter
    where [ Input: Stream<Item = char> ]
    {
        use Direction::*;
        use Delimiter::*;
        satisfy_map(|c: char| match c {
            '(' => Some(Paren(Left)),
            ')' => Some(Paren(Right)),
            ';' => Some(Semicolon),
            ',' => Some(Comma),
            _   => None,
        })
    }
}

const OPERATORS: &'static str = "+-*/<>=";

parser!{
    pub fn operator['a, Input]()(Input) -> Token<'a>
    where [ Input: RangeStream<Item = char, Range = &'a str> ]
    {
        use Reserved::*;
        use Token::*;
        let is_operator = |c: char| OPERATORS.chars().any(|r| r == c);
        take_while1(is_operator).flat_map(|tok| match tok {
            "+" => Ok(Keyword(Add)),
            "-" => Ok(Keyword(Sub)),
            "*" => Ok(Keyword(Mult)),
            "=" => Ok(Keyword(Equal)),
            "<" => Ok(Keyword(LessThan)),
            "=>" => Ok(Keyword(Arrow)),
            _ => panic!("lexing failure"), // TODO
        })
    }
}

parser!{
    pub fn token['a, Input]()(Input) -> Token<'a>
    where [ Input: RangeStream<Item = char, Range = &'a str> ]
    {
        use Token::*;
        choice!(
            eof().map(|_| EndOfFile),
            spaces(),
            delimiter().map(Delim),
            number().map(Lit),
            alphabetic(),
            operator()
        )
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
                // println!("{:?} : {:?}", token, rest);
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

    #[test]
    fn tokenizer_unit3() {
        let tokenizer = Tokenizer::new("let val x=1 in x<1 end");
        let result = run_tokenizer(tokenizer);
        let should = vec![
            Keyword(Let), Space(1), Keyword(Val), Space(1),
            Name("x"), Keyword(Equal), Lit(Integer(1)), Space(1),
            Keyword(In), Space(1), Name("x"), Keyword(LessThan),
            Lit(Integer(1)), Space(1), Keyword(End)
        ];
        assert_eq!(result, Ok(should))
    }


}
