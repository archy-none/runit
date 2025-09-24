use indexmap::IndexMap;
use name::Name;

fn main() {
    println!("fn main() {}", build().unwrap())
}

fn build() -> Option<String> {
    let mut ctx = Context {
        refcnt: IndexMap::new(),
    };
    let code = include_str!("../example.prs");
    let ast = Expr::parse(code)?;
    ast.visit(&mut ctx)?;
    dbg!(&ctx);
    ast.compile(&mut ctx)
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Context {
    refcnt: IndexMap<Name, usize>,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
enum Expr {
    Let(Box<Expr>, Box<Expr>, Box<Expr>),
    Variable(Name),
    String(String),
}

impl Expr {
    fn compile(&self, ctx: &mut Context) -> Option<String> {
        match self {
            Expr::Let(name, value, expr) => match *name.clone() {
                Expr::Variable(name) => Some(format!(
                    "{{\n\tlet {name} = {};\n{};\n}}",
                    value.compile(ctx)?,
                    expr.compile(ctx)?
                        .lines()
                        .map(|line| format!("\t{line}"))
                        .collect::<Vec<_>>()
                        .join("\n")
                )),
                _ => None,
            },
            Expr::Variable(name) if ctx.refcnt.get(name) == Some(&1) => Some(name.to_string()),
            Expr::Variable(name) => {
                *ctx.refcnt.get_mut(name)? -= 1;
                Some(format!("{name}.clone()"))
            }
            Expr::String(value) => Some(format!("String::from(\"{value}\")")),
        }
    }

    fn visit(&self, ctx: &mut Context) -> Option<()> {
        match self {
            Expr::Let(name, value, expr) => match *name.clone() {
                Expr::Variable(name) => {
                    if !ctx.refcnt.contains_key(&name) {
                        ctx.refcnt.insert(name, 0);
                    }
                    value.visit(ctx)?;
                    expr.visit(ctx)?;
                }
                _ => return None,
            },
            Expr::Variable(name) => *ctx.refcnt.get_mut(name)? += 1,
            Expr::String(_) => {}
        };
        Some(())
    }

    fn parse(source: &str) -> Option<Self> {
        let source = source.trim();
        if let Some(token) = source.strip_prefix("let ") {
            let (name, token) = token.split_once("=")?;
            let (value, expr) = token.split_once("in")?;
            Some(Expr::Let(
                Box::new(Expr::parse(name)?),
                Box::new(Expr::parse(value)?),
                Box::new(Expr::parse(expr)?),
            ))
        } else if let Some(str) = source
            .strip_prefix("\"")
            .and_then(|token| token.strip_suffix("\""))
        {
            Some(Expr::String(str.to_string()))
        } else if let Some(name) = Name::new(source) {
            Some(Expr::Variable(name))
        } else {
            None
        }
    }
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
