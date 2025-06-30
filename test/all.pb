//// ALL LANGUAGE FEATURES ////

// Variables
{
	var integer: i32 = 42;
	print("42 = "integer.to_str());
	integer += 8;
	print("50 = "integer.to_str());
}

// Constants
{
	const my_constant: bool = true;
	print("true = "my_constant.to_str());
}

// Functions
{
	fn double(x: i32) -> i32
		= x * 2;
	fn longer_func() -> i32 {
		var a = double(5);
		a
	}
	print("10 = "longer_func().to_str());
}

// Compile-time execution
{
	fn hello_world() {
		print("Hello from compile time!");
	}

	const { hello_world(); }
}

// Structs and associated functions
{
	const Point = struct {
		x: i32,
		y: i32,

		fn to_str(&self) -> String
			= "("self.x", "self.y")";
	};

	var my_point = Point {
		.x = 4,
		.y = 10,
	};

	print("(4, 10) ="my_point.to_str());
}

// Enums
{
	const HTTPError = enum {
		FileNotFound,
		Disconnected,
		Forbidden,
		Other(u16),

		fn to_str(&self) -> string
			= match self {
				FileNotFound => "file not found",
				Disconnected => "disconnected",
				Forbidden => "forbidden",
				Other(code) => "http error "code.to_str(),
			};
	};

	var error = HTTPError.other(418);

	print("418 = "error.to_str());
}

// Pointers
{
	fn increase(number: @ptr(i32)) {
		*number += 1;
	}

	var my_number = 1;
	increase(&my_number);
	print("2 = "my_number);

	const constant_number = 4;
	assert_type(&constant_number, @ptr(const i32));
}

// First-class types / generics
{
	fn Either(A: type, B: type) -> type
		= enum {
			A(A),
			B(B),
		};
	const A = Either(infer, infer).A;
	const B = Either(infer, infer).B;
	
	var user_input = B("Hello world");
	print("'Hello World' = "debug_fmt(user_input));
}

// Optionals
{
	fn get_username(id: u32) -> Option(string)
		= if id == 1 {
			Some("mageowl")
		} else {
			None
		};

	print("'mageowl' = "debug_fmt(get_username(1)));
	print("null = "debug_fmt(get_username(2)));
}

// Import and export
{
	const Pt = @import("./point.pb");

	var my_point = Pt.new(10, 2);
	print("(10, 2) = "my_point.to_str());
}

// Reflection
{
	const MyStruct = struct {
		property: i32,
	};
	
	print("struct { ... } = "@reflect(MyStruct).to_str())
}
