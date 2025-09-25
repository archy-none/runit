fn main() {
	println!("{}", {
		fn r#_add(r#_a: isize, r#_b: isize) -> isize {
			let r#_c = r#_a;
			r#_a + r#_c
		}
		r#_add(5isize + 2isize, r#_add(3isize, 3isize))
	});
}

