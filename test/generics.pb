type Peekable<T> = [
	peek: fn() -> ref Option<T>,
	next: fn() -> Option<T>,
];

fn peekable:<T>(iterator: Iter<T>) -> Peekable<T> = {
	mut queue: Option<T> = [];
	[
		next = iterator.next(),
		peek = fn() -> ref Option<T> = {
			match ref queue {
				some: ref T = some;
				none: ref [] = {
					queue = iterator.next();
					ref queue
				};
			}
		},
	]
};
