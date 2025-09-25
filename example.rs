fn main() {
	let mut r#_a = String::from("Hello");
	{
		let r#_b = 100usize;
		{
			r#_a = String::from("Bye");
			{
				let r#_c = r#_a.clone();
				r#_a;
			};
		};
	};
}
