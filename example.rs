fn main() {
	println!("{}", {
		fn r#_add(r#_a: isize, r#_b: isize) -> isize {
			let r#_c = r#_a;
			r#_b + r#_c
		}
		let r#_n = r#_add(5isize + 2isize, r#_add(3isize, 3isize));
		let mut r#_c = String::from("Hello! ");
		let r#_d = r#_c.clone();
		r#_c = r#_c.clone().repeat(r#_n as usize);
		r#_c
	});
}

