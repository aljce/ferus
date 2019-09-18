use std::fmt;
use combine::{
    Parser, Stream, satisfy, satisfy_map, choice, between,
    chainl1, attempt, optional, value,
};

pub mod pretty;
pub mod eval;

use crate::lexer::{Literal, Direction, Delimiter, Reserved, Token};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum UnaryOp {
    Not,
    Fst,
    Snd,
}

impl UnaryOp {
    fn precedence(self) -> usize {
        use UnaryOp::*;
        match self {
            Not => 9,
            Fst => 9,
            Snd => 9,
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnaryOp::*;
        let name = match *self {
            Not => "not",
            Fst => "fst",
            Snd => "snd",
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Equal,
    LessThan,
    OrElse,
    AndAlso,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryOp::*;
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
        };
        write!(f, "{}", name)
    }
}

impl BinaryOp {
    fn precedence(self) -> usize {
        use BinaryOp::*;
        match self {
            Add => 4,
            Sub => 4,
            Mult => 5,
            Div => 5,
            Mod => 5,
            Equal => 3,
            LessThan => 3,
            OrElse => 1,
            AndAlso => 2,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Var(&'a str),
    Lit(Literal<'a>),
    Unary {
        operation: UnaryOp,
        child: Box<Expr<'a>>,
    },
    Binary {
        left: Box<Expr<'a>>,
        operation: BinaryOp,
        right: Box<Expr<'a>>,
    },
    IfThenElse {
        condition: Box<Expr<'a>>,
        if_branch: Box<Expr<'a>>,
        else_branch: Box<Expr<'a>>,
    },
    Tuple {
        fst: Box<Expr<'a>>,
        snd: Box<Expr<'a>>,
    },
    Let {
        name: &'a str,
        binder: Box<Expr<'a>>,
        body: Box<Expr<'a>>,
    },
    Lambda {
        name: &'a str,
        body: Box<Expr<'a>>,
    },
    App {
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn parens<F>(f: &mut fmt::Formatter, inner: usize, outer: usize, cb: F) -> fmt::Result
        where F: FnOnce(&mut fmt::Formatter) -> fmt::Result
        {
            if inner < outer {
                write!(f, "(")?;
                cb(f)?;
                write!(f, ")")
            } else {
                cb(f)
            }
        }
        fn draw<'a>(f: &mut fmt::Formatter, expr: &Expr<'a>, prec: usize) -> fmt::Result {
            use Expr::*;
            match expr {
                Var(name) => write!(f, "{}", name),
                Lit(lit) => write!(f, "{}", lit),
                Unary{ operation, child } => {
                    let op_prec = operation.precedence();
                    parens(f, op_prec, prec, |g| {
                        write!(g, "{} ", operation)?;
                        draw(g, child, op_prec)
                    })
                },
                Binary{ left, operation, right } => {
                    let op_prec = operation.precedence();
                    parens(f, op_prec, prec, |g| {
                        draw(g, left, op_prec)?;
                        write!(g, " {} ", operation)?;
                        draw(g, right, op_prec)
                    })
                },
                IfThenElse{ condition, if_branch, else_branch } => {
                    parens(f, 0, prec, |g| {
                        write!(g, "if ")?;
                        draw(g, condition, 0)?;
                        write!(g, " then ")?;
                        draw(g, if_branch, 0)?;
                        write!(g, " else ")?;
                        draw(g, else_branch, 0)
                    })
                },
                Tuple{ fst, snd } => {
                    write!(f, "(")?;
                    draw(f, fst, 0)?;
                    draw(f, snd, 0)?;
                    write!(f, ")")
                },
                Let{ name, binder, body } => {
                    parens(f, 0, prec, |g| {
                        write!(g, "let val {} = ", name)?;
                        draw(g, binder, 0)?;
                        write!(g, " in ")?;
                        draw(g, body, 0)?;
                        write!(g, " end")
                    })
                },
                Lambda{ name, body } => {
                    parens(f, 0, prec, |g| {
                        write!(g, "fn {} => ", name)?;
                        draw(g, body, 0)
                    })
                },
                App{ left, right } => {
                    parens(f, 9, prec, |g| {
                        draw(g, left, 10)?;
                        write!(g, " ")?;
                        draw(g, right, 10)
                    })
                },
            }
        }
        draw(f, self, 0)
    }
}

parser!{
    pub fn token['a, Input](t: Token<'a>)(Input) -> ()
    where [ Input: Stream<Item = Token<'a>> ]
    {
        satisfy(|cur: Token<'a>| cur == *t).map(|_| ())
    }
}

parser!{
    pub fn name['a, Input]()(Input) -> &'a str
    where [ Input: Stream<Item = Token<'a>> ]
    {
        satisfy_map(|t| match t {
            Token::Name(n) => Some(n),
            _ => None
        })
    }
}

parser!{
    pub fn space['a, Input]()(Input) -> ()
    where [ Input: Stream<Item = Token<'a>> ]
    {
        satisfy_map(|t| match t {
            Token::Space(n) if 0 < n => Some(()),
            _ => None
        })
    }
}


parser!{
    #[derive(Clone)]
    pub struct Lex;
    pub fn lex['a, Input, P](f: P)(Input) -> P::Output
    where [ Input: Stream<Item = Token<'a>>, P: Parser<Input> ]
    {
        between(optional(space()), optional(space()), f)
    }
}

// <prog> ::= <expn>EOF
// <expn> ::= let val<name> = <expn> in <expn> end | if <expn> then <expn> else <expn>
// <expn> ::= fn <name> => <expn> | <disj>
// <disj> ::= <disj> orelse <conj> | <conj>
// <conj> ::= <conj> andalso <cmpn> | <cmpn>
// <cmpn> ::= <addn> = <addn> | <addn> < <addn> | <addn>
// <addn> ::= <addn> + <mult> | <addn> - <mult> | <mult>
// <mult> ::= <mult> * <unar> | <mult> div <unar> | <mult> mod <unar> | <unar>
// <unar> ::= not <appn> | fst <appn> | snd <appn> | <appn>
// <appn> ::= <appn> <atom> | <atom>
// <atom> ::= <name> | <numn> | true | false | ( <expn> ) | ( <expn> , <expn> )
// <name> ::= a | b | c | ...
// <numn> ::= 0 | 1 | 2 | ...
parser!{
    pub fn prog['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        (expn(), token(Token::EndOfFile)).map(|(expr, _)| expr)
    }
}

parser!{
    pub fn expn['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        use Token::*;
        use Expr::*;
        let let_val = struct_parser!{
            Let {
                _: token(Keyword(Reserved::Let)),
                _: space(),
                _: token(Keyword(Reserved::Val)),
                _: space(),
                name: name(),
                _: lex(token(Keyword(Reserved::Equal))),
                binder: expn().map(Box::new),
                _: token(Keyword(Reserved::In)),
                body: expn().map(Box::new),
                _: token(Keyword(Reserved::End)),
            }
        };
        let if_then_else = struct_parser!{
            IfThenElse {
                _: token(Keyword(Reserved::If)),
                condition: expn().map(Box::new),
                _: token(Keyword(Reserved::Then)),
                if_branch: expn().map(Box::new),
                _: token(Keyword(Reserved::Else)),
                else_branch: expn().map(Box::new)
            }
        };
        let lambda = struct_parser!{
            Lambda {
                _: token(Keyword(Reserved::Fn)),
                _: space(),
                name: name(),
                _: space(),
                _: token(Keyword(Reserved::Arrow)),
                body: expn().map(Box::new),
            }
        };
        lex(choice!(let_val, if_then_else, lambda, disj()))
    }
}

parser!{
    pub fn disj['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        let binary = satisfy_map(|t| match t {
            Token::Keyword(Reserved::OrElse) => Some(BinaryOp::OrElse),
            _ => None
        }).map(|op| move |left, right| Expr::Binary {
            left: Box::new(left),
            operation: op,
            right: Box::new(right)
        });
        chainl1(conj(), binary)
    }
}

parser!{
    pub fn conj['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        let binary = satisfy_map(|t| match t {
            Token::Keyword(Reserved::AndAlso) => Some(BinaryOp::AndAlso),
            _ => None
        }).map(|op| move |left, right| Expr::Binary {
            left: Box::new(left),
            operation: op,
            right: Box::new(right)
        });
        chainl1(cmp(), binary)
    }
}

parser!{
    pub fn cmp['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        use Expr::*;
        let comparison = satisfy_map(|t| match t {
            Token::Keyword(Reserved::Equal) => Some(BinaryOp::Equal),
            Token::Keyword(Reserved::LessThan) => Some(BinaryOp::LessThan),
            _ => None
        });
        let binary = struct_parser!{
            Binary {
                left: add().map(Box::new),
                operation: comparison,
                right: add().map(Box::new),
            }
        };
        choice!(attempt(binary), add())
    }
}

parser!{
    pub fn add['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        let binary = satisfy_map(|t| match t {
            Token::Keyword(Reserved::Add) => Some(BinaryOp::Add),
            Token::Keyword(Reserved::Sub) => Some(BinaryOp::Sub),
            _ => None
        }).map(|op| move |left, right| Expr::Binary {
            left: Box::new(left),
            operation: op,
            right: Box::new(right)
        });
        chainl1(mult(), binary)
    }
}


parser!{
    pub fn mult['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        let binary = satisfy_map(|t| match t {
            Token::Keyword(Reserved::Mult) => Some(BinaryOp::Mult),
            Token::Keyword(Reserved::Div) => Some(BinaryOp::Div),
            Token::Keyword(Reserved::Mod) => Some(BinaryOp::Mod),
            _ => None
        }).map(|op| move |left, right| Expr::Binary {
            left: Box::new(left),
            operation: op,
            right: Box::new(right)
        });
        chainl1(nega(), binary)
    }
}

parser!{
    pub fn nega['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        use Expr::*;
        let operation = satisfy_map(|t| match t {
            Token::Keyword(Reserved::Not) => Some(UnaryOp::Not),
            Token::Keyword(Reserved::Fst) => Some(UnaryOp::Fst),
            Token::Keyword(Reserved::Snd) => Some(UnaryOp::Snd),
            _ => None
        });
        let unary = struct_parser!{
            Unary {
                operation: operation,
                _: space(),
                child: appn().map(Box::new)
            }
        };
        choice!(attempt(unary), appn())
    }
}

parser!{
    pub fn appn['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        let binary = value(|left, right| Expr::App {
            left: Box::new(left),
            right: Box::new(right)
        });
        chainl1(atom(), binary).message("function application")
    }
}

parser!{
    pub fn atom['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        use Direction::*;
        use Expr::*;
        let variable = name().map(Var);
        let literal = satisfy_map(|t| match t {
            Token::Lit(lit) => Some(Lit(lit)),
            _ => None
        });
        let paren = |dir| token(Token::Delim(Delimiter::Paren(dir)));
        let nested = between(paren(Left), paren(Right), lex(expn()));
        let tuple = struct_parser!{
            Tuple {
                _: paren(Left),
                fst: lex(expn().map(Box::new)),
                _: token(Token::Delim(Delimiter::Comma)),
                snd: lex(expn().map(Box::new)),
                _: paren(Right),
            }
        };
        lex(choice!(variable, literal, attempt(nested), tuple))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Tokenizer};

    #[test]
    fn parse_success_unit() {
        let tests = vec![
            "1+2*3<4 andalso true",
            "let val x = let val a = 2 in fn b => a end in let val y=1 in fn z => x end end",
            "fst (1+3,5)"
        ];
        for test in tests {
            let res = prog().parse(Tokenizer::new(test));
            assert!(res.is_ok());
        }
    }

    #[test]
    fn parse_roundtrip_unit() {
        let tests = vec![
            "(1 + 2) * 3",
            "let val x = 1 in let val y = 2 in x + y end end",
            "fn x => fn y => x (x (x y))"
        ];
        for test in tests {
            let res = prog().parse(Tokenizer::new(test));
            assert_eq!(Ok(test.to_string()), res.map(|(e, _)| e.to_string()))
        }
    }
}
