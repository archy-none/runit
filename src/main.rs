use indexmap::IndexMap;
use name::Name;

fn main() {
    println!("fn main() {}", build().unwrap())
}

fn build() -> Result<String, String> {
    let mut ctx = Context {
        mutenv: IndexMap::new(),
        typenv: IndexMap::new(),
        refcnt: IndexMap::new(),
    };
    let code = include_str!("../example.prs");
    let ast = Expr::parse(code)?;
    ast.infer(&mut ctx)?;
    ast.visit(&mut ctx)?;
    ast.compile(&mut ctx)
}

pub const SPACE: &str = " ";

#[derive(Clone, Debug, PartialEq, Eq)]
struct Context {
    mutenv: IndexMap<Name, bool>,
    typenv: IndexMap<Name, Type>,
    refcnt: IndexMap<Name, usize>,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
enum Expr {
    Let(Box<Expr>, Box<Expr>, Box<Expr>),
    Operator(String, Vec<Expr>),
    Variable(Name),
    String(String),
    Integer(isize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    String,
    Integer,
}

impl Type {
    fn is_object(&self) -> bool {
        matches!(self, Type::String)
    }
}

impl Expr {
    fn compile(&self, ctx: &mut Context) -> Option<String> {
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
                    Some(format!("{{\n\t{statement}{name} = {value};\n{expr};\n}}"))
                }
                _ => None,
            },
            Expr::Variable(name) => {
                if let Some(cnt) = ctx.refcnt.get_mut(name) {
                    if *cnt == 1 {
                        Some(name.to_string())
                    } else {
                        *cnt -= 1;
                        Some(format!("{name}.clone()"))
                    }
                } else {
                    Some(name.to_string())
                }
            }
            Expr::String(value) => Some(format!("String::from(\"{value}\")")),
            Expr::Integer(value) => Some(format!("{value}usize")),
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
                _ => return Err("d".to_owned()),
            },
            Expr::Variable(name) => {
                if let Some(cnt) = ctx.refcnt.get_mut(name) {
                    *cnt += 1
                }
            }
            _ => {}
        };
        Ok(())
    }

    fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        match self {
            Expr::Let(name, value, expr) => match *name.clone() {
                Expr::Variable(name) => {
                    let valtyp = value.infer(ctx)?;
                    if let Some(vartyp) = ctx.typenv.get(&name) {
                        if *vartyp == valtyp {
                            ctx.mutenv.insert(name, true);
                        } else {
                            return Err("()".to_owned());
                        }
                    } else {
                        ctx.typenv.insert(name, valtyp);
                    }
                    expr.infer(ctx)
                }
                _ => return Err("()".to_owned());
            },
            Expr::Variable(name) => ok!(ctx.typenv.get(name).cloned()),
            Expr::Integer(_) => Ok(Type::Integer),
            Expr::String(_) => Ok(Type::String),
        }
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
        } else if let Some(str) = source
            .strip_prefix("\"")
            .and_then(|token| token.strip_suffix("\""))
        {
            Ok(Expr::String(str.to_string()))
        } else if let Ok(name) = source.parse() {
            Ok(Expr::Integer(name))
        } else if let Some(name) = Name::new(source) {
            Ok(Expr::Variable(name))
        } else {
            let tokens: Vec<String> = tokenize(source, SPACE.as_ref())?;
            // Parsing is from right to left because operator is left-associative
            let binopergen = |n: usize| {
                let operator = tokens.get(n)?;
                let lhs = tokens.get(..n)?.join(SPACE);
                let rhs = tokens.get(n + 1..)?.join(SPACE);
                Some(match operator.as_str() {
                    "+" => Op::Add(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "-" => Op::Sub(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "*" => Op::Mul(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "/" => Op::Div(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "%" => Op::Mod(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    ">>" => Op::Shr(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "<<" => Op::Shl(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "==" => Op::Eql(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "!=" => Op::Neq(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "<" => Op::Lt(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    ">" => Op::Gt(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    ">=" => Op::GtEq(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "<=" => Op::LtEq(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "&" => Op::BAnd(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "|" => Op::BOr(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "^" => Op::XOr(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "&&" => Op::LAnd(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    "||" => Op::LOr(Expr::parse(lhs)?, Expr::parse(rhs)?),
                    ":" => Op::Cast(Expr::parse(lhs)?, Type::parse(rhs)?),
                    _ => return None,
                })
            };
            let unaryopergen = || {
                let op = tokens.first()?.trim();
                let token = tokens.get(1..)?.join(SPACE);
                Some(match op {
                    "~" => Op::BNot(Expr::parse(token)?),
                    "!" => Op::LNot(Expr::parse(token)?),
                    "-" => {
                        let token = Expr::parse(token)?;
                        Op::Sub(
                            Expr::Operator(Box::new(Op::Sub(token.clone(), token.clone()))),
                            token,
                        )
                    }
                    _ => return None,
                })
            };
            let suffixopergen = || {
                let op = tokens.last()?.trim();
                let token = tokens.get(..tokens.len() - 1)?.join(SPACE);
                Some(match op {
                    "?" => Op::NullCheck(Expr::parse(token)?),
                    "!" => Op::Nullable(Type::parse(token)?),
                    _ => return None,
                })
            };
            for i in 2..tokens.len() {
                if let Some(op) = binopergen(tokens.len().checked_sub(i)?) {
                    return Some(op);
                }
            }
            if let Some(op) = suffixopergen() {
                return Some(op);
            }
            if let Some(op) = unaryopergen() {
                return Some(op);
            }
            None
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
