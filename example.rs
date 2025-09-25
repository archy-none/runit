fn main() {
	println!("{}", {
		fn r#_inc(r#_a: isize, r#_b: isize) -> isize {
			r#_a + r#_b
		}
		r#_inc(5isize, 3isize)
	});
}

