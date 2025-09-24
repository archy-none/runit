fn main() {
	let r#_a = String::from("Hello");
	{
		let r#_b = r#_a;
		{
			let r#_c = r#_b.clone();
			r#_b;
		};
	};
}
