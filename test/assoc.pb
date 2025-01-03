type Person = [
	first_name: str,
	last_name: str,
	age: num,

	into: fn { (self: Person) -> str; (self: Person) -> num; },
];

fn Person_into(self: Person) -> str
	= self.first_name;

fn Person([first_name: str, last_name: str = "", age: num]) -> Person = {
	let self = [
		with first_name,
		with last_name,
		into = Person_into,
	];
	self
};

type Into<T> = [
	into: fn(Self) -> T,
];

let owen = Person([
	first_name = "Owen",
	last_name = "L",
	age = 15,
]);

print(owen);
print(owen.age);
