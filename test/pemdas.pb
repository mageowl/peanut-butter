
print(2 + 4 / 2); // 4
print(-3 / 3); // -1

// eventually, a macro for making types
// let tbl = makety!([ a = true, b = 10, c = [ "one", "two" ] ]);
let tbl = {
	type Temp = [ a = num, b = bool, c = [ str, str ] ];
	Temp: [ a = true, b = 10, c = [ "one", "two" ] ]
};
print(tbl.a); // true
print(tbl.c[0]); // one
