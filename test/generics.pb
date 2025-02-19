use std.process.[with exit];

fn unwrap:<T>(val: Option<T>) -> T
	= match val {
		some: T = some;
		[] = {
			println("called unwrap on none.");
			exit();
		};
	};

let my_value: num | [] = 8;
unwrap(my_value); // implicitly T = num
unwrap:<str>([]);
