fn bind_print()

let my_fn = decorate(fn() = println("first"));
decorate(fn() = println("second"));
my_fn();
