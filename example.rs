fn main() {
	let r#_a = String::from("Hello");
	{
		let r#_b = r#_a.clone();
		{
			let r#_c = r#_b.clone();
			{
				let r#_d = r#_b;
				{
					let r#_a = r#_d;
					{
						let r#_d = r#_a.clone();
						{
							let r#_c = r#_a;
							r#_c;
						};
					};
				};
			};
		};
	};
}
