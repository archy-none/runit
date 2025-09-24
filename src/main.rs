use indexmap::IndexMap;
use name::Name;

fn main() {
    println!("fn main() {}", build().unwrap())
}

fn build() -> Option<String> {
    let mut ctx = Context {
        refcnt: IndexMap::new(),
    };
    let ast = Expr::Let(
        Box::new(Expr::Variable(Name::new("a")?)),
        Box::new(Expr::String("Hello".to_owned())),
        Box::new(Expr::Let(
            Box::new(Expr::Variable(Name::new("b")?)),
            Box::new(Expr::Variable(Name::new("a")?)),
            Box::new(Expr::Variable(Name::new("a")?)),
        )),
    );
    ast.visit(&mut ctx)?;
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
        dbg!((&self, &ctx));
        match self {
            Expr::Let(name, value, expr) => match *name.clone() {
                Expr::Variable(name) => Some(format!(
                    "{{\n\tlet {name} = {};\n{}\n}}",
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
                    ctx.refcnt.insert(name, 0);
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
            write!(f, "r#{}", self.0)
        }
    }
}
