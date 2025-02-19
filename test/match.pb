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

fn complex() -> [num | str, num | str] = [2, 4];
match complex() {
	[a: num | str, b: str] = println("aB");
	_ = println("lol");
};

