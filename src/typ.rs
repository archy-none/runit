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
                let tokens: Vec<String> = tokenize(source, SPACE.as_ref())?;
                let ret = ok!(tokens.first())?.trim();
                let args: Vec<String> = tokenize(&ok!(tokens.get(1..))?.join(SPACE), ",")?;
                Ok(Type::Function(
                    args.iter()
                        .map(|arg| Type::parse(&arg))
                        .collect::<Result<Vec<_>, String>>()?,
                    Box::new(Type::parse(&ret)?),
                ))
            }
        }
    }

    fn compile(&self) -> String {
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
