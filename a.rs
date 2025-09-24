fn main() {
	let r#a = String::from("Hello");
	{
		let r#b = r#a;
		{
			let r#c = r#b.clone();
			r#b;
		};
	};
}
