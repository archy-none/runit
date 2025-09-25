pub mod name {
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
            if !first_char.is_ascii_lowercase() {
                return None;
            }
            if !chars.all(UnicodeXID::is_xid_continue) {
                return None;
            }
            if !chars.all(|c| c.is_ascii_lowercase()) {
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
#[macro_export]
macro_rules! indent {
    ($v: expr) => {
        $v.lines()
            .map(|line| format!("\t{line}"))
            .collect::<Vec<_>>()
            .join("\n")
    };
}
