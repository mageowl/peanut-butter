type Person = [
	name: string,
	age: number,
	hobbies: List<string>,
];

let hobbies = ["coding", "gay", "minecraft"]; // Implicitly List<string>
let owen: Person = [
	name = "owen",
	age = 14,
	with hobbies
];

fn greet(name: string) -> none = {
	print("Hello, " + name + "!");
};

greet("World");
print(greet);

