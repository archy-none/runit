fn main() {
	let r#_a = String::from("Hello");
	{
		let r#_b = 100usize;
		{
			let r#_b = r#_a.clone();
			r#_a;
		};
	};
}
