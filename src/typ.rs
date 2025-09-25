use crate::*;

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Type {
    String,
    Integer,
    Bool,
    Function(Vec<Type>, Box<Type>),
}

impl Type {
    pub fn is_object(&self) -> bool {
        matches!(self, Type::String)
    }

    pub fn parse(source: &str) -> Result<Type, String> {
        let source = source.trim();
        match source {
            "Int" => Ok(Type::Integer),
            "Str" => Ok(Type::String),
            "Bool" => Ok(Type::Bool),
            _ => {
                let (source, ret) = ok!(source.split_once("->"))?;
                let args: Vec<String> = tokenize(source, ",")?;
                if args.len() == 0 {
                    return Err(String::new());
                }
                Ok(Type::Function(
                    args.iter()
                        .map(|arg| Type::parse(&arg))
                        .collect::<Result<Vec<_>, String>>()?,
                    Box::new(Type::parse(&ret)?),
                ))
            }
        }
    }

    pub fn compile(&self) -> String {
        match self {
            Type::Integer => "isize".to_string(),
            Type::String => "String".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Function(args, ret) => format!(
                "fn({}) -> {}",
                args.iter()
                    .map(Type::compile)
                    .collect::<Vec<_>>()
                    .join(", "),
                ret.compile()
            ),
        }
    }
}
