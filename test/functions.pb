fn greet(name: str) = {
	println("Hello, " name);
};
fn call_twice(cb: fn()) = {
	cb();
	cb();
};

fn decorate(func: fn()) -> fn()
	= fn() = func();

mut i = 0;
greet("world");
call_twice(fn() = {
	i += 1;
	println("loop " num_to_str(i))
});
