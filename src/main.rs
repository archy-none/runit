use indexmap::IndexMap;
use lex::name::Name;
use lex::tokenize;

mod lex;
mod typ;
use typ::Type;

fn main() {
    println!(
        "fn main() {{\n\tprintln!(\"{{}}\", {{\n{}\n\t}});\n}}\n",
        indent!(indent!(build().unwrap()))
    );
}

fn build() -> Result<String, String> {
    let mut ctx = Context::default();
    let code = include_str!("../example.tr");
    let ast = Expr::parse(code)?;
    ast.infer(&mut ctx)?;
    ast.visit(&mut ctx)?;
    eprintln!("{ctx:#?}");
    ast.compile(&mut ctx)
}

pub const SPACE: &str = " ";

#[derive(Clone, Debug, PartialEq, Eq, Default)]
struct Context {
    mutenv: IndexMap<Name, bool>,
    functx: IndexMap<Name, Context>,
    typenv: IndexMap<Name, Type>,
    typexp: IndexMap<Expr, Type>,
    refcnt: IndexMap<Name, usize>,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
enum Expr {
    Proto(Name, Type, Box<Expr>),
    Let(Box<Expr>, Box<Expr>, Box<Expr>),
    Operator(String, Vec<Expr>),
    Function(Name, Vec<Expr>),
    Variable(Name),
    String(String),
    Integer(isize),
    Bool(bool),
    Kind(Type),
}

impl Expr {
    fn compile(&self, ctx: &mut Context) -> Result<String, String> {
        match self {
            Expr::Let(name, value, expr) => Ok(match *name.clone() {
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
                    format!("{statement}{name} = {value};\n")
                }
                Expr::Function(name, params) => {
                    let func_ctx = ok!(ctx.functx.get_mut(&name))?;
                    let Type::Function(annos, ret) = ok!(ctx.typenv.get(&name))?.clone() else {
                        return Err(format!("can't call to non-function object: {name}"));
                    };
                    let value = indent!(value.compile(func_ctx)?);
                    let ret = ret.compile();
                    let mut args = vec![];
                    for (param, anno) in params.iter().zip(annos) {
                        if let Expr::Variable(name) = param {
                            args.push(format!("{name}: {}", anno.compile()));
                        }
                    }
                    let args = args.join(", ");
                    format!("fn {name}({args}) -> {ret} {{\n{value}\n}}\n")
                }
                _ => todo!(),
            } + &expr.compile(ctx)?),
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
                            Type::String => Ok(format!("[{lhs}, {rhs}].concat()")),
                            Type::Bool => Ok(format!("{lhs} || {rhs}")),
                            _ => todo!(),
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
                            _ => todo!(),
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
            Expr::Kind(_) => todo!(),
            Expr::Proto(_, _, expr) => expr.compile(ctx),
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
                Expr::Function(name, _) => {
                    let func_ctx = ok!(ctx.functx.get_mut(&name))?;
                    value.visit(func_ctx)?;
                    expr.visit(ctx)?;
                }
                _ => todo!(),
            },
            Expr::Variable(name) => {
                if let Some(cnt) = ctx.refcnt.get_mut(name) {
                    *cnt += 1
                }
            }
            Expr::Operator(_, terms) | Expr::Function(_, terms) => {
                for term in terms {
                    term.visit(ctx)?;
                }
            }
            Expr::Proto(_, _, expr) => expr.visit(ctx)?,
            _ => {}
        };
        Ok(())
    }

    fn infer(&self, ctx: &mut Context) -> Result<Type, String> {
        let result = match self {
            Expr::Proto(name, typ, expr) => {
                ctx.typenv.insert(name.clone(), typ.clone());
                expr.infer(ctx)?
            }
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
                Expr::Function(name, params) => {
                    let mut func_ctx = ctx.clone();
                    let Type::Function(annos, ret) = ok!(ctx.typenv.get(&name))?.clone() else {
                        return Err(format!(
                            "there's no prototype declaration so can't define function: {name}"
                        ));
                    };
                    if params.len() != annos.len() {
                        return Err(format!(
                            "arguments length not matched between prototype declaration and function definition: {} != {}",
                            params.len(),
                            annos.len()
                        ));
                    }
                    for (param, anno) in params.iter().zip(annos) {
                        if let Expr::Variable(name) = param {
                            func_ctx.typenv.insert(name.clone(), anno);
                        }
                    }
                    let value = value.infer(&mut func_ctx)?;
                    ctx.functx.insert(name, func_ctx);
                    if *ret != value {
                        return Err(format!(
                            "type not matched between prototype declaration and result of type inference: {ret:?} != {value:?}"
                        ));
                    }
                    expr.infer(ctx)?
                }
                _ => return Err("invalid binding".to_owned()),
            },
            Expr::Function(name, args) => {
                let Type::Function(params, ret) = ok!(ctx.typenv.get(name))?.clone() else {
                    return Err(format!("can't call to non-function object: {name}"));
                };
                if params.len() != args.len() {
                    return Err(format!(
                        "arguments length not matched between function definition and passed from call: {} != {}",
                        params.len(),
                        args.len()
                    ));
                }
                for (arg, param) in args.iter().zip(params) {
                    let arg = arg.infer(ctx)?;
                    if arg != param {
                        return Err(format!(
                            "passed argument did not match to type that's function expects: {arg:?} != {param:?}"
                        ));
                    }
                }
                *ret.clone()
            }
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
            Expr::Kind(_) => todo!(),
        };
        ctx.typexp.insert(self.to_owned(), result.clone());
        Ok(result)
    }

    fn parse(source: &str) -> Result<Self, String> {
        let source = source.trim();
        if let Some(token) = source.strip_prefix("let ") {
            let (name, token) = once!(token, "=")?;
            let (value, expr) = once!(&token, "in")?;
            Ok(Expr::Let(
                Box::new(Expr::parse(name.trim())?),
                Box::new(Expr::parse(&value)?),
                Box::new(Expr::parse(&expr)?),
            ))
        } else if let Some(token) = source.strip_prefix("proto ") {
            let (name, token) = once!(token, "=")?;
            let (value, expr) = once!(&token, "in")?;
            Ok(Expr::Proto(
                ok!(Name::new(name.trim()))?,
                Type::parse(&value)?,
                Box::new(Expr::parse(&expr)?),
            ))
        } else {
            let tokens: Vec<String> = tokenize(source, SPACE.as_ref())?;
            let funccall = || {
                let name = ok!(tokens.first())?.trim();
                let args: Vec<String> = tokenize(&ok!(tokens.get(1..))?.join(SPACE), ",")?;
                if args.len() == 0 {
                    return Err(String::new());
                }
                Ok::<Expr, String>(Expr::Function(
                    ok!(Name::new(name))?,
                    args.iter()
                        .map(|arg| Expr::parse(&arg))
                        .collect::<Result<Vec<_>, String>>()?,
                ))
            };
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
            if let Ok(func) = funccall() {
                return Ok(func);
            }

            if let Some(str) = source
                .strip_prefix("\"")
                .and_then(|token| token.strip_suffix("\""))
            {
                Ok(Expr::String(str.to_string()))
            } else if let Some(expr) = source
                .strip_prefix("(")
                .and_then(|token| token.strip_suffix(")"))
            {
                Expr::parse(expr)
            } else if let Ok(int) = source.parse() {
                Ok(Expr::Integer(int))
            } else if let Ok(bool) = source.parse() {
                Ok(Expr::Bool(bool))
            } else if let Ok(typ) = Type::parse(source) {
                Ok(Expr::Kind(typ))
            } else if let Some(name) = Name::new(source) {
                Ok(Expr::Variable(name))
            } else {
                Err(format!("unknown expression: {source}"))
            }
        }
    }
}
