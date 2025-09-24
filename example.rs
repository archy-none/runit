fn main() {
	let mut r#_a = String::from("Hello");
	{
		let mut r#_b = r#_a.clone();
		r#_a;
	};
}
