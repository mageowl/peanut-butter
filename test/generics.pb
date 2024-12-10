type Option<T> = enum [
	[T],
	[],
];
fn some<T>(value: T) -> Option<T> = [value];
fn none<T>() -> Option<T> = [];
fn Option#is_some(self) -> bool = match self {
	[_] = true,
	[] = false,
};

some(32); //: Option<num>
// none(); error, could not infer generics.
let maybe_number: Option<num> = none();
print(maybe_number.is_some()); // false

