fn main() {
	println!("{}", {
		fn r#_add(r#_a: isize, r#_b: isize) -> isize {
			r#_a + r#_b
		}
		r#_add(5isize + 2isize, 3isize)
	});
}

