use std::collections::HashMap;
use crate::lexer::{Literal};
use crate::expr::{UnaryOp, BinaryOp, Expr};

pub struct Env<'a> {
    context: HashMap<&'a str, Expr<'a>>
}

impl<'a> Env<'a> {
    fn new() -> Env<'a> {
        Env { context: HashMap::new() }
    }
    fn lookup(&self, name: &'a str) -> Option<&Expr<'a>> {
        self.context.get(name)
    }
    fn extend<A, F>(&mut self, name: &'a str, expr: Expr<'a>, cb: F) -> A
    where F: FnOnce(&mut Env<'a>) -> A
    {
        self.context.insert(name, expr);
        let res = cb(self);
        self.context.remove(name);
        res
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    Boolean,
    Integer,
}

#[derive(Debug)]
pub enum Error<'a> {
    NotFound(&'a str),
    TypeError{ exp: Expr<'a>, typ: Type },
}

impl<'a> Expr<'a> {
    pub fn eval_ctx(&self, env1: &mut Env<'a>) -> Result<Expr<'a>, Error> {
        use Literal::*;
        use UnaryOp::*;
        use BinaryOp::*;
        use Expr::*;
        use Error::*;
        match self {
            Var(name) => {
                // match env1.lookup(name) {
                //     Some(expr) => expr.eval_ctx(env1),
                //     None => Err(NotFound(name)),
                // }
                let expr = env1.lookup(name).ok_or(NotFound(name))?.clone();
                Ok(expr)
                // expr.eval_ctx(env1)
            },
            Lit(lit) => Ok(Lit(*lit)),
            Unary{ operation, child } => {
                match operation {
                    Not => {
                        match child.eval_ctx(env1)? {
                            Lit(Boolean(b)) => Ok(Lit(Boolean(!b))),
                            _ => Err(TypeError { exp: *child.clone(), typ: Type::Boolean })
                        }
                    },
                }
            },
            // Binary{ left, operation, right } => {
            //     panic!()
            // },
            // IfThenElse{ condition, if_branch, else_branch } => {
            //     panic!()
            // },
            Let{ name, binder, child } => {
                env1.extend(name, *binder.clone(), |env2| child.eval_ctx(env2))
            },
            _ => panic!()
        }
    }
    pub fn eval(&self) -> Result<Expr<'a>, Error> {
        // let mut env = Env::new();
        // self.eval_ctx(&mut env)
        Ok(self.clone())
    }
}
