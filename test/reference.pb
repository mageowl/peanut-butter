fn add_one(var: ref num) = {
	var.ref += 1;
};

mut my_num = 5;
add_one(ref my_num);
println(num_to_str(my_num));
