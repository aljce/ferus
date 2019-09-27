use std::fmt;
use std::collections::HashMap;
use crate::lexer::{Literal};
use crate::expr::{UnaryOp, BinaryOp, Expr};

#[derive(Debug, Clone)]
pub struct Closure<'a> {
    formal: &'a str,
    body: Expr<'a>,
    context: Env<'a>,
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Unit,
    Integer(i64),
    Boolean(bool),
    String(&'a str),
    Tuple{ fst: Box<Value<'a>>, snd: Box<Value<'a>> },
    Function(Closure<'a>),
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match *self {
            Unit => write!(f, "()"),
            Integer(i) => write!(f, "{}", i),
            Boolean(b) => write!(f, "{}", b),
            String(s) => write!(f, "{}", s),
            Tuple{ ref fst, ref snd } => write!(f, "({}, {})", fst, snd),
            Function(Closure{ formal, ref body, ref context }) => {
                if context.empty() {
                    write!(f, "fn {} => {}", formal, body)
                } else {
                    write!(f, "fn {} => {} [{}]", formal, body, context)
                }
            },
        }
    }
}

impl<'a> Value<'a> {
    fn unit(self) -> Result<(), Error<'a>> {
        use Value::*;
        use Error::*;
        match self {
            Unit => Ok(()),
            _ => Err(TypeError{ expr: self, should: Type::Unit })
        }
    }
    fn boolean(self) -> Result<bool, Error<'a>> {
        use Value::*;
        use Error::*;
        match self {
            Boolean(b) => Ok(b),
            _ => Err(TypeError{ expr: self, should: Type::Boolean })
        }
    }
    fn integer(self) -> Result<i64, Error<'a>> {
        use Value::*;
        use Error::*;
        match self {
            Integer(i) => Ok(i),
            _ => Err(TypeError{ expr: self, should: Type::Integer })
        }
    }
    fn tuple(self) -> Result<(Value<'a>, Value<'a>), Error<'a>> {
        use Value::*;
        use Error::*;
        match self {
            Tuple{ fst, snd } => Ok((*fst, *snd)),
            _ => Err(TypeError{ expr: self, should: Type::Tuple })
        }
    }
    fn function(self) -> Result<Closure<'a>, Error<'a>> {
        use Value::*;
        use Error::*;
        match self {
            Function(c) => Ok(c),
            _ => Err(TypeError{ expr: self, should: Type::Function })
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    Unit,
    Boolean,
    Integer,
    Function,
    Tuple,
}

#[derive(Debug)]
pub enum Error<'a> {
    NotFound(&'a str),
    TypeError{ expr: Value<'a>, should: Type },
}

impl<'a> Literal<'a> {
    pub fn into_value(self) -> Value<'a> {
        use Value::*;
        match self {
            Literal::Unit       => Unit,
            Literal::Integer(i) => Integer(i),
            Literal::Boolean(b) => Boolean(b),
            Literal::String(s)  => String(s),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Env<'a> {
    context: HashMap<&'a str, Value<'a>>
}

impl<'a> fmt::Display for Env<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.context.iter();
        if let Some((name, value)) = iter.next() {
            write!(f, "{} -> {}", name, value)?;
            for (name, value) in iter {
                write!(f, ", {} -> {}", name, value)?;
            }
        }
        Ok(())
    }
}

impl<'a> Env<'a> {
    fn new() -> Env<'a> {
        Env { context: HashMap::new() }
    }
    fn empty(&self) -> bool {
        self.context.is_empty()
    }
    fn lookup(&self, name: &'a str) -> Option<&Value<'a>> {
        self.context.get(name)
    }
    fn extend<A, F>(&mut self, name: &'a str, value: Value<'a>, cb: F) -> A
    where F: FnOnce(&mut Env<'a>) -> A
    {
        let old = self.context.insert(name, value);
        let res = cb(self);
        // variable shadowing
        match old {
            Some(old_value) => self.context.insert(name, old_value),
            None => self.context.remove(name),
        };
        res
    }
}

impl<'a> Expr<'a> {
    pub fn eval_ctx(self, env1: &mut Env<'a>) -> Result<Value<'a>, Error<'a>> {
        use UnaryOp::*;
        use BinaryOp::*;
        use Expr::*;
        use Value::*;
        use Error::*;
        match self {
            Var(name) => match env1.lookup(name) {
                Some(value) => Ok(value.clone()),
                None => Err(NotFound(name))
            },
            Lit(lit) => Ok(lit.into_value()),
            Unary{ operation, child } => match operation {
                Not => {
                    let b = child.eval_ctx(env1)?.boolean()?;
                    Ok(Boolean(!b))
                },
                Fst => {
                    let tuple = child.eval_ctx(env1)?.tuple()?;
                    Ok(tuple.0)
                },
                Snd => {
                    let tuple = child.eval_ctx(env1)?.tuple()?;
                    Ok(tuple.1)
                },
                Print => {
                    let val = child.eval_ctx(env1)?;
                    println!("{}", val);
                    Ok(Unit)
                },
            },
            Binary{ left, operation, right } => match operation {
                Add => {
                    let left_val = left.eval_ctx(env1)?.integer()?;
                    let right_val = right.eval_ctx(env1)?.integer()?;
                    Ok(Integer(left_val + right_val))
                },
                Sub => {
                    let left_val = left.eval_ctx(env1)?.integer()?;
                    let right_val = right.eval_ctx(env1)?.integer()?;
                    Ok(Integer(left_val - right_val))
                },
                Mult => {
                    let left_val = left.eval_ctx(env1)?.integer()?;
                    let right_val = right.eval_ctx(env1)?.integer()?;
                    Ok(Integer(left_val * right_val))
                },
                Div => {
                    let left_val = left.eval_ctx(env1)?.integer()?;
                    let right_val = right.eval_ctx(env1)?.integer()?;
                    Ok(Integer(left_val / right_val))
                },
                Mod => {
                    let left_val = left.eval_ctx(env1)?.integer()?;
                    let right_val = right.eval_ctx(env1)?.integer()?;
                    Ok(Integer(left_val % right_val))
                },
                Equal => {
                    let left_val = left.eval_ctx(env1)?.integer()?;
                    let right_val = right.eval_ctx(env1)?.integer()?;
                    Ok(Boolean(left_val == right_val))
                },
                LessThan => {
                    let left_val = left.eval_ctx(env1)?.integer()?;
                    let right_val = right.eval_ctx(env1)?.integer()?;
                    Ok(Boolean(left_val < right_val))
                },
                OrElse => {
                    let left_val = left.eval_ctx(env1)?.boolean()?;
                    // rust short circuits even under the result monad :)
                    Ok(Boolean(left_val || right.eval_ctx(env1)?.boolean()?))
                },
                AndAlso => {
                    let left_val = left.eval_ctx(env1)?.boolean()?;
                    Ok(Boolean(left_val && right.eval_ctx(env1)?.boolean()?))
                },
            },
            IfThenElse{ condition, if_branch, else_branch } => {
                if condition.eval_ctx(env1)?.boolean()? {
                    if_branch.eval_ctx(env1)
                } else {
                    else_branch.eval_ctx(env1)
                }
            },
            Expr::Tuple{ fst, snd } => {
                let fst_val = fst.eval_ctx(env1)?;
                let snd_val = snd.eval_ctx(env1)?;
                Ok(Value::Tuple{ fst: Box::new(fst_val), snd: Box::new(snd_val) })
            },
            Let{ name, binder, body } => {
                let binder_val = binder.eval_ctx(env1)?;
                env1.extend(name, binder_val, |env2| body.eval_ctx(env2))
            },
            Lambda{ name, body } => {
                Ok(Function(Closure{ formal: name, body: *body, context: env1.clone() }))
            },
            App{ left, right } => {
                let Closure{ formal, body, mut context } = left.eval_ctx(env1)?.function()?;
                let right_val = right.eval_ctx(env1)?;
                context.extend(formal, right_val, |env2| body.eval_ctx(env2))
            },
            Seq{ left, right } => {
                left.eval_ctx(env1)?.unit()?;
                right.eval_ctx(env1)
            },
        }
    }
    pub fn eval(self) -> Result<Value<'a>, Error<'a>> {
        let mut env = Env::new();
        self.eval_ctx(&mut env)
    }
}

#[cfg(test)]
mod tests {
    use combine::Parser;
    use crate::lexer::{Tokenizer};
    use crate::expr::{prog};

    #[test]
    fn eval_unit() {
        fn test(fun: &'static str, input: &'static str, output: i64) {
            let y_comb = "fn f => (fn x => f (fn v => x x v)) (fn x => f (fn v => x x v))";
            let expr_str = format!("({}) ({}) {}", y_comb, fun, input);
            let (expr, _) = prog().parse(Tokenizer::new(expr_str.as_str())).unwrap();
            let evaled = expr.eval().and_then(|v| v.integer()).unwrap();
            assert_eq!(evaled, output)
        }
        let factorial = "fn factorial => fn n => if n = 0 then 1 else n * factorial (n - 1)";
        test(factorial, "10", 3628800);
        let fib = "fn fib => fn n => if n < 2 then n else fib (n - 1) + fib (n - 2)";
        test(fib, "6", 8);
        let power = r#"
          fn pow => fn base => fn exp =>
            if exp = 0
              then 1
              else if exp mod 2 = 0
                     then pow (base * base) (exp div 2)
                     else base * pow (base * base) (exp div 2)
        "#;
        test(power, "2 16", 2_i64.pow(16));
    }
}
