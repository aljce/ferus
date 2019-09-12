use std::iter;
use std::fmt;
use combine::{
    Stream, satisfy, satisfy_map, choice, between,
    chainl1, chainr1, attempt
};

use crate::lexer::{Literal, Direction, Reserved, Token};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum UnaryOp {
    Not
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnaryOp::*;
        let name = match *self {
            Not => "not",
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
    Exp,
    Equal,
    LessThan,
    OrElse,
    AndAlso,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryOp::*;
        let name = match *self {
            Add => "(+)",
            Sub => "(-)",
            Mult => "(*)",
            Div => "div",
            Mod => "mod",
            Exp => "(**)",
            Equal => "(=)",
            LessThan => "(<)",
            OrElse => "orelse",
            AndAlso => "andalso",
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    Var(&'a str),
    Lit(Literal<'a>),
    Unary {
        operation: UnaryOp,
        child: Box<Expr<'a>>
    },
    Binary {
        left: Box<Expr<'a>>,
        operation: BinaryOp,
        right: Box<Expr<'a>>
    },
    IfThenElse {
        condition: Box<Expr<'a>>,
        if_branch: Box<Expr<'a>>,
        else_branch: Box<Expr<'a>>
    },
    Let {
        name: &'a str,
        binder: Box<Expr<'a>>,
        child: Box<Expr<'a>>
    },
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn padding(indent: usize) -> String {
            iter::repeat(' ').take(indent).collect()
        }
        fn draw_tree<'a>(f: &mut fmt::Formatter, expr: &Expr<'a>, indent: usize) -> fmt::Result {
            use Expr::*;
            match expr {
                Var(name) => write!(f, "{}{}\n", padding(indent), name),
                Lit(lit) => write!(f, "{}{}\n", padding(indent), lit),
                Unary{ operation, child } => {
                    write!(f, "{}{}\n", padding(indent), operation)?;
                    draw_tree(f, child, indent + 3)
                },
                Binary{ left, operation, right } => {
                    write!(f, "{}{}\n", padding(indent), operation)?;
                    draw_tree(f, left, indent + 3)?;
                    draw_tree(f, right, indent + 3)
                }
                IfThenElse{ condition, if_branch, else_branch } => {
                    write!(f, "{}if then else\n", padding(indent))?;
                    draw_tree(f, condition, indent + 3)?;
                    draw_tree(f, if_branch, indent + 3)?;
                    draw_tree(f, else_branch, indent + 3)
                }
                Let{ name, binder, child } => {
                    write!(f, "{}let\n", padding(indent))?;
                    write!(f, "{}{}\n", padding(indent + 3), name)?;
                    draw_tree(f, binder, indent + 3)?;
                    draw_tree(f, child, indent + 3)
                }
            }
        }
        draw_tree(f, self, 0)
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

// <expn> ::= let val <name> = <expn> in <expn> end | if <expn> then <expn> else <expn> | <disj>
// <disj> ::= <disj> orelse <conj> | <conj>
// <conj> ::= <conj> andalso <cmpn> | <cmpn>
// <cmpn> ::= <addn> = <addn> | <addn> < <addn> | <addn>
// <addn> ::= <addn> + <mult> | <addn> - <mult> | <mult>
// <mult> ::= <mult> * <pown> | <mult> div <pown> | <mult> mod <pown> | <pown>
// <pown> ::= <nega> ** <pown> | <nega>
// <nega> ::= not <atom> | <atom>
// <atom> ::= <name> | <numn> | true | false | ( <expn> )
// <name> ::= a | b | c | ...
// <numn> ::= 0 | 1 | 2 | ...
parser!{
    pub fn expn['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        use Token::*;
        use Expr::*;
        let let_val = struct_parser!{
            Let {
                _: token(Keyword(Reserved::Let)),
                _: token(Keyword(Reserved::Val)),
                name: name(),
                _: token(Keyword(Reserved::Equal)),
                binder: expn().map(Box::new),
                _: token(Keyword(Reserved::In)),
                child: expn().map(Box::new),
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
        choice((let_val, if_then_else, disj()))
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
        choice((attempt(binary), add()))
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
        chainl1(pown(), binary)
    }
}

parser!{
    pub fn pown['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        let binary = satisfy_map(|t| match t {
            Token::Keyword(Reserved::Exp) => Some(BinaryOp::Exp),
            _ => None
        }).map(|op| move |left, right| Expr::Binary {
            left: Box::new(left),
            operation: op,
            right: Box::new(right)
        });
        chainr1(nega(), binary)
    }
}

parser!{
    pub fn nega['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        use Expr::*;
        let operation = satisfy_map(|t| match t {
            Token::Keyword(Reserved::Not) => Some(UnaryOp::Not),
            _ => None
        });
        let unary = struct_parser!{
            Unary {
                operation: operation,
                child: atom().map(Box::new)
            }
        };
        choice((unary, atom()))
    }
}

parser!{
    pub fn atom['a, Input]()(Input) -> Expr<'a>
    where [ Input: Stream<Item = Token<'a>> ]
    {
        let variable = satisfy_map(|t| match t {
            Token::Name(name) => Some(Expr::Var(name)),
            _ => None
        });
        let literal = satisfy_map(|t| match t {
            Token::Lit(lit) => Some(Expr::Lit(lit)),
            _ => None
        });
        let nested = between(
            token(Token::Paren(Direction::Left)),
            token(Token::Paren(Direction::Right)),
            expn()
        );
        choice!(variable, literal, nested)
    }
}
