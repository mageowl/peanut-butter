fn num_or_str(b: bool) -> num | str
	= if b {
		"hello world"
	} else {
		42
	};

match num_or_str(true) {
	s: str = println(s);
	n: num = println(to_str(n));
};
