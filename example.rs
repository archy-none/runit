fn main() {
	println!("{}", {
		fn r#_add(r#_a: isize, r#_b: isize) -> isize {
			let r#_c = r#_a;
			r#_b + r#_c
		}
		fn r#_main(r#_a: String, r#_b: isize) -> String {
			let r#_n = r#_add(1isize + 2isize, r#_add(r#_b, 3isize));
			let r#_c = String::from("Hello, ");
			let r#_d = r#_c.clone() + &r#_a + &String::from("! ");
			let r#_e = r#_c + &r#_d.clone();
			r#_e + &r#_d.repeat(r#_n as usize)
		}
		r#_main(String::from("world"), 3isize)
	});
}

