fn my_range(min: num, max: num) -> Iter<num> = {
	mut i = min - 1;
	fn() -> num | [] = {
		i += 1;
		if i == max {
			[]
		} else {
			i
		}
	}
};

fn some_or_empty(x: num | bool | str | []) -> str
	= match x {
		x: num | bool | str = to_str(x);
		[] = "";
	};

for i in my_range(0, 3) {
	println("For iteration #"to_str(i));
};

let iter = my_range(0, 10);
println("Custom next call #"some_or_empty(iter()));
println("Custom next call #"some_or_empty(iter()));
println("Custom next call #"some_or_empty(iter()));
println("Custom next call #"some_or_empty(iter()));

let rest = collect(iter);
println("Rest of itererator: "fmt_tbl(rest));
