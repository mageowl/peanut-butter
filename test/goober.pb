fn get_username(id: u32) -> Option(string)
	= if id == 1 {
		Some("mageowl")
	} else {
		None
	};

print("'mageowl' = "debug_fmt(get_username(1)));
print("null = "debug_fmt(get_username(2)));

