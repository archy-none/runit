fn main() {
	let r#_a = String::from("Hello");
	{
		let r#_b = r#_a.clone();
		{
			let r#_c = r#_b;
			{
				let r#_a = String::from("Bye");
				{
					let r#_d = r#_a.clone();
					r#_a;
				};
			};
		};
	};
}
