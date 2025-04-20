type Shape = [
	area: fn(self) -> i32,
];

type Rectangle = [
	width: i32,
	height: i32,
] & Shape;

fn Rectangle(width: i32, height: i32) -> Rectangle {
	[
		width, height,
		fn area(self) -> i32 {
			self.width * self.height
		},
	]
}

let rect = Rectangle(10, 10);
assert_eq(rect#area(), 100);
