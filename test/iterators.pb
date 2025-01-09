type Iter<State, Type> = [
	next: fn(ref State) -> Type,
	initial_state: State,
];

type RangeState = [i = num, max = num];
fn range(min: num, max: num) -> Iter<RangeState, num>
	= [
		next = fn(state: ref RangeState) -> num | [] {
			let i = state.i;
			if state.i == state.max {
				[]
			} else {
				state.i += 1;
				i
			}
		},
		init = [i = min, with max],
	];

for i in range(0, 10) {
	print("Iteration " i);
};
