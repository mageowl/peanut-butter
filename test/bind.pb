fn bind_print(text: str) -> fn()
	= fn() = println(text);

let my_fn = bind_print("correct");
bind_print("incorrect");
my_fn();
