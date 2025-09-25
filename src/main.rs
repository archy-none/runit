use indexmap::IndexMap;
use name::Name;

fn main() {
    println!("fn main() {}", build().unwrap())
}

fn build() -> Result<String, String> {
    let mut ctx = Context {
        mutenv: IndexMap::new(),
        typenv: IndexMap::new(),
        typexp: IndexMap::new(),
        refcnt: IndexMap::new(),
    };
    let code = include_str!("../example.prs");
    let ast = Expr::parse(code)?;
    ast.infer(&mut ctx)?;
    ast.visit(&mut ctx)?;
    dbg!(&ctx);
    ast.compile(&mut ctx)
}

pub const SPACE: &str = " ";

#[derive(Clone, Debug, PartialEq, Eq)]
struct Context {
    mutenv: IndexMap<Name, bool>,
    typenv: IndexMap<Name, Type>,
    typexp: IndexMap<Expr, Type>,
    refcnt: IndexMap<Name, usize>,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
enum Expr {
    Let(Box<Expr>, Box<Expr>, Box<Expr>),
    Operator(String, Vec<Expr>),
    Function(String, Vec<Expr>),
    Variable(Name),
    String(String),
    Integer(isize),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    String,
    Integer,
    Bool,
}

impl Type {
    fn is_object(&self) -> bool {
        matches!(self, Type::String)
    }
}

impl Expr {
    fn compile(&self, ctx: &mut Context) -> Result<String, String> {
        match self {
            Expr::Let(name, value, expr) => match *name.clone() {
                Expr::Variable(name) => {
                    let mut statement = String::new();
                    if let Some(is_initial) = ctx.mutenv.get_mut(&name) {
                        if *is_initial {
                            *is_initial = false;
                            statement.push_str("let mut ");
                        }
                    } else {
                        statement.push_str("let ");
                    };
                    let value = value.compile(ctx)?;
                    let expr = expr
                        .compile(ctx)?
                        .lines()
                        .map(|line| format!("\t{line}"))
                        .collect::<Vec<_>>()
                        .join("\n");
                    Ok(format!("{{\n\t{statement}{name} = {value};\n{expr};\n}}"))
                }
                _ => todo!(),
            },
            Expr::Variable(name) => {
                if let Some(cnt) = ctx.refcnt.get_mut(name) {
                    if *cnt == 1 {
                        Ok(name.to_string())
                    } else {
                        *cnt -= 1;
                        Ok(format!("{name}.clone()"))
                    }
                } else {
                    Ok(name.to_string())
                }
            }
            Expr::Function(name, args) => Ok(format!(
                "{name}({})",
                args.iter()
                    .map(|x| x.compile(ctx))
                    .collect::<Result<Vec<String>, String>>()?
                    .join(", ")
            )),
            Expr::String(value) => Ok(format!("String::from(\"{value}\")")),
            Expr::Integer(value) => Ok(format!("{value}isize")),
            Expr::Bool(value) => Ok(format!("{value}")),
            Expr::Operator(op, terms) => match op.as_str() {
                "+" => match terms.as_slice() {
                    [lhs, rhs] => {
                        let lhs = lhs.compile(ctx)?;
                        let rhs = rhs.compile(ctx)?;
                        match ok!(ctx.typexp.get(self))? {
                            Type::Integer => Ok(format!("{lhs} + {rhs}")),
                            Type::String => Ok(format!("{lhs} + &{rhs}")),
                            Type::Bool => Ok(format!("{lhs} || {rhs}")),
                        }
                    }
                    _ => todo!(),
                },
                "-" => match terms.as_slice() {
                    [lhs, rhs] => {
                        let lhs = lhs.compile(ctx)?;
                        let rhs = rhs.compile(ctx)?;
                        match ok!(ctx.typexp.get(self))? {
                            Type::Integer => Ok(format!("{lhs} - {rhs}")),
                            Type::String => Ok(format!("{lhs}.replace({rhs}, \"\")")),
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                },
                "*" => match terms.as_slice() {
                    [lhs, rhs] => {
                        let lhs = lhs.compile(ctx)?;
                        let rhs = rhs.compile(ctx)?;
                        match ok!(ctx.typexp.get(self))? {
                            Type::Integer => Ok(format!("{lhs} * {rhs}")),
                            Type::String => Ok(format!("{lhs}.repeat({rhs} as usize)")),
                            Type::Bool => Ok(format!("{lhs} && {rhs}")),
                        }
                    }
                    _ => todo!(),
                },
                "/" => match terms.as_slice() {
                    [lhs, rhs] => {
                        let lhs = lhs.compile(ctx)?;
                        let rhs = rhs.compile(ctx)?;
                        match ok!(ctx.typexp.get(self))? {
                            Type::Integer => Ok(format!("{lhs} / {rhs}")),
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                },
                "==" => match terms.as_slice() {
                    [lhs, rhs] => {
                        let lhs = lhs.compile(ctx)?;
                        let rhs = rhs.compile(ctx)?;
                        Ok(format!("{lhs} {op} {rhs}"))
                    }
                    _ => todo!(),
                },
                "!=" => match terms.as_slice() {
                    [lhs, rhs] => {
                        let lhs = lhs.compile(ctx)?;
                        let rhs = rhs.compile(ctx)?;
                        Ok(format!("{lhs} {op} {rhs}"))
                    }
                    _ => todo!(),
                },
                "<" => match terms.as_slice() {
                    [lhs, rhs] => {
                        let lhs = lhs.compile(ctx)?;
                        let rhs = rhs.compile(ctx)?;
                        Ok(format!("{lhs} {op} {rhs}"))
                    }
                    _ => todo!(),
                },
                ">" => match terms.as_slice() {
                    [lhs, rhs] => {
                        let lhs = lhs.compile(ctx)?;
                        let rhs = rhs.compile(ctx)?;
                        Ok(format!("{lhs} {op} {rhs}"))
                    }
                    _ => todo!(),
                },
                "<=" => match terms.as_slice() {
                    [lhs, rhs] => {
                        let lhs = lhs.compile(ctx)?;
                        let rhs = rhs.compile(ctx)?;
                        Ok(format!("{lhs} {op} {rhs}"))
                    }
                    _ => todo!(),
                },
                ">=" => match terms.as_slice() {
                    [lhs, rhs] => {
                        let lhs = lhs.compile(ctx)?;
                        let rhs = rhs.compile(ctx)?;
                        Ok(format!("{lhs} {op} {rhs}"))
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            },
        }
    }

    fn visit(&self, ctx: &mut Context) -> Result<(), String> {
        match self {
            Expr::Let(name, value, expr) => match *name.clone() {
                Expr::Variable(name) => {
                    if let Some(var) = ctx.typenv.get(&name) {
                        if !ctx.refcnt.contains_key(&name) && var.is_object() {
                            ctx.refcnt.insert(name, 0);
                        }
                    }
                    value.visit(ctx)?;
                    expr.visit(ctx)?;
                }
                _ => todo!(),
            },
            Expr::Variable(name) => {
                if let Some(cnt) = ctx.refcnt.get_mut(name) {
                    *cnt += 1
                }
            }
            Expr::Operator(_, terms) => {
                for term in terms {
                    term.visit(ctx)?;
                }
            }
            _ => {}
        };
        Ok(())
    }

    fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        let result = match self {
            Expr::Let(name, value, expr) => match *name.clone() {
                Expr::Variable(name) => {
                    let valtyp = value.infer(ctx)?;
                    if let Some(vartyp) = ctx.typenv.get(&name) {
                        if *vartyp == valtyp {
                            ctx.mutenv.insert(name, true);
                        } else {
                            return Err(
                                "can't re-assign to variable by other typed value".to_owned()
                            );
                        }
                    } else {
                        ctx.typenv.insert(name, valtyp);
                    }
                    expr.infer(ctx)?
                }
                _ => return Err("invalid binding".to_owned()),
            },
            Expr::Variable(name) => ok!(ctx.typenv.get(name).cloned())?,
            Expr::Integer(_) => Type::Integer,
            Expr::String(_) => Type::String,
            Expr::Bool(_) => Type::Bool,
            Expr::Operator(op, terms) => {
                let typs = terms
                    .iter()
                    .map(|i| i.infer(ctx))
                    .collect::<Result<Vec<_>, String>>()?;
                let err = Err(format!("can't apply `{op}` operator to terms: {typs:?}"));
                match op.as_str() {
                    "+" => match typs.as_slice() {
                        [Type::Integer, Type::Integer] => Type::Integer,
                        [Type::String, Type::String] => Type::String,
                        [Type::Bool, Type::Bool] => Type::Bool,
                        _ => return err,
                    },
                    "-" => match typs.as_slice() {
                        [Type::Integer, Type::Integer] => Type::Integer,
                        [Type::String, Type::String] => Type::String,
                        _ => return err,
                    },
                    "*" => match typs.as_slice() {
                        [Type::Integer, Type::Integer] => Type::Integer,
                        [Type::String, Type::Integer] => Type::String,
                        [Type::Bool, Type::Bool] => Type::Bool,
                        _ => return err,
                    },
                    "/" => match typs.as_slice() {
                        [Type::Integer, Type::Integer] => Type::Integer,
                        _ => return err,
                    },
                    "==" => match typs.as_slice() {
                        [lhs, rhs] if lhs == rhs => Type::Bool,
                        _ => return err,
                    },
                    "!=" => match typs.as_slice() {
                        [lhs, rhs] if lhs == rhs => Type::Bool,
                        _ => return err,
                    },
                    "<" => match typs.as_slice() {
                        [lhs, rhs] if lhs == rhs => Type::Bool,
                        _ => return err,
                    },
                    ">" => match typs.as_slice() {
                        [lhs, rhs] if lhs == rhs => Type::Bool,
                        _ => return err,
                    },
                    "<=" => match typs.as_slice() {
                        [lhs, rhs] if lhs == rhs => Type::Bool,
                        _ => return err,
                    },
                    ">=" => match typs.as_slice() {
                        [lhs, rhs] if lhs == rhs => Type::Bool,
                        _ => return err,
                    },
                    _ => todo!(),
                }
            }
        };
        ctx.typexp.insert(self.to_owned(), result.clone());
        Ok(result)
    }

    fn parse(source: &str) -> Result<Self, String> {
        let source = source.trim();
        if let Some(token) = source.strip_prefix("let ") {
            let (name, token) = ok!(token.split_once("="))?;
            let (value, expr) = ok!(token.split_once("in"))?;
            Ok(Expr::Let(
                Box::new(Expr::parse(name)?),
                Box::new(Expr::parse(value)?),
                Box::new(Expr::parse(expr)?),
            ))
        } else {
            let tokens: Vec<String> = tokenize(source, SPACE.as_ref())?;
            let binopergen = |n: usize| {
                let op = ok!(tokens.get(n))?;
                let lhs = &ok!(tokens.get(..n))?.join(SPACE);
                let rhs = &ok!(tokens.get(n + 1..))?.join(SPACE);
                let terms = vec![Expr::parse(lhs)?, Expr::parse(rhs)?];
                Ok(match op.as_str() {
                    "+" | "-" | "*" | "/" | "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        Expr::Operator(op.to_owned(), terms)
                    }
                    _ => return Err(format!("invalid binary operator: {op}")),
                })
            };
            let unaryopergen = || {
                let op = ok!(tokens.first())?.trim();
                let token = Expr::parse(&ok!(tokens.get(1..))?.join(SPACE))?;
                Ok(match op {
                    "-" | "!" => Expr::Operator(op.to_owned(), vec![token]),
                    _ => return Err(format!("invalid unary operator: {op}")),
                })
            };
            for i in 2..tokens.len() {
                if let Ok(op) = ok!(tokens.len().checked_sub(i)).and_then(binopergen) {
                    return Ok(op);
                }
            }
            if let Ok(op) = unaryopergen() {
                return Ok(op);
            }

            if let Some(str) = source
                .strip_prefix("\"")
                .and_then(|token| token.strip_suffix("\""))
            {
                Ok(Expr::String(str.to_string()))
            } else if let Ok(name) = source.parse() {
                Ok(Expr::Integer(name))
            } else if let Ok(name) = source.parse() {
                Ok(Expr::Bool(name))
            } else if let Some(name) = Name::new(source) {
                Ok(Expr::Variable(name))
            } else {
                Err(format!("unknown expression: {source}"))
            }
        }
    }
}

pub fn tokenize(input: &str, delimiter: &str) -> Result<Vec<String>, String> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: usize = 0;
    let mut in_quote = false;
    let mut is_escape = false;

    let chars = input.chars().collect::<Vec<char>>();
    let mut index = 0;

    while index < chars.len() {
        let c = chars[index];
        if is_escape {
            current_token.push(c);
            is_escape = false;
        } else {
            match c {
                '(' | '{' | '[' if !in_quote => {
                    current_token.push(c);
                    in_parentheses += 1;
                }
                ')' | '}' | ']' if !in_quote => {
                    current_token.push(c);
                    in_parentheses.checked_sub(1).map(|x| in_parentheses = x);
                }
                '"' | '\'' | '`' => {
                    in_quote = !in_quote;
                    current_token.push(c);
                }
                '\\' if in_quote => {
                    current_token.push(c);
                    is_escape = true;
                }
                _ => {
                    if input.get(index..index + delimiter.len()) == Some(delimiter) {
                        if in_parentheses != 0 || in_quote || is_escape {
                            current_token.push_str(delimiter);
                        } else if !current_token.is_empty() {
                            tokens.push(current_token.clone());
                            current_token.clear();
                        }
                        index += delimiter.len();
                        continue;
                    } else {
                        current_token.push(c);
                    }
                }
            }
        }
        index += 1
    }

    // Syntax error check
    if is_escape || in_quote || in_parentheses != 0 {
        return Err("nested structure is not closed".to_owned());
    }
    if !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }
    Ok(tokens)
}

#[macro_export]
macro_rules! ok {
    ($v: expr) => {
        if let Some(v) = $v {
            Ok(v)
        } else {
            Err("invalid token".to_string())
        }
    };
}

mod name {
    use std::fmt;
    use unicode_xid::UnicodeXID;

    const RESERVED: [&str; 2] = ["let", "in"];

    #[derive(Clone, Debug, PartialEq, Hash, Eq)]
    pub struct Name(String);

    impl Name {
        pub fn new(name: &str) -> Option<Name> {
            if name.is_empty() {
                return None;
            }
            let mut chars = name.chars();
            let first_char = chars.next().unwrap();
            if !UnicodeXID::is_xid_start(first_char) {
                return None;
            }
            if !chars.all(UnicodeXID::is_xid_continue) {
                return None;
            }
            if RESERVED.contains(&name) {
                return None;
            }
            if !name.is_ascii() {
                return None;
            }
            Some(Name(name.to_owned()))
        }
    }

    impl fmt::Display for Name {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "r#_{}", self.0)
        }
    }
}
