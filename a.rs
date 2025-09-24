fn main() {
    let r#a = String::from("Hello");
    {
        let r#b = r#a;
        println!("{}", r#a)
    }
}
