/*
fn add_one(var: ref num) = {
	println("adding one");
	var.ref += 1;
};

mut my_num = 5;
println("my number is "num_to_str(my_num));
add_one(ref my_num);
println("my number is "num_to_str(my_num));
*/

fn trick(tbl: [value: str]) = {
	mut clone = tbl;
	clone.value = "tricky hehe";
};

let immutable = [value = "stolid"];
trick(immutable);
println(immutable.value);
