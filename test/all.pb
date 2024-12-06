use "";

type Person = [
	name: string,
	age: number,
	hobbies: list<string>,
];

fn Person#greet(self, name: string) = {
	print("Hello, " name "! I am " self.name);
};

let hobbies = ["coding", "gay", "minecraft"]; // Implicitly list<string>
let owen = Person#[
	name = "owen",
	age = 14,
	with hobbies
];

fn greet(name: string) = {
	print("Hello, " + name + "!");
};

greet("World");
print(greet); // [greet(name: string) -> none]
