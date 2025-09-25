fn main() { let mut r#_a = String::from("Hello") + &String::from("World");
let r#_b = 10isize / 2isize;
r#_a = r#_a.clone().repeat(r#_b as usize);
let r#_c = r#_a.clone();
r#_a }
