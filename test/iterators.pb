fn my_range(min: num, max: num) -> Iter<num> = {
	mut i = min - 1;
	[
		next = fn() -> Option<num> = {
			i += 1;
			if i >= max {
				[]
			} else {
				i
			}
		}
	]
};

fn some_or_empty(x: num | bool | str | []) -> str
	= match x {
		x: num | bool | str = to_str(x);
		[] = "none";
	};

for i in my_range(0, 3) {
	println("For iteration #"to_str(i));
};

// for i in [0, 1, 2] {
// 	println("item = "to_str(i));
// };
