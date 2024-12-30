fn bind_print(text: str) -> fn()
	= fn() = println(text);

let my_fn = bind_print("first");
bind_print("second");
my_fn();
