const Pt = struct {
	x: i32,
	y: i32,

	fn new(x: i32, y: i32) -> Self {
		return Self { x, y };
	}

	fn add(a: Self, b: Self) -> Self {
		return Self {
			.x = a.x + b.x,
			.y = a.y + b.y,
		};
	}

	fn to_str(&self) -> string
		= "("self.x.to_str()", "self.y.to_str()")";
};

return Pt;
