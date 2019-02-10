
SBOX = "E4D12FB83A6C5907"
PERM = {1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 16};

P[v_] := Permute[v, PERM];

F[s_, x_] := Module[{sbox},
	(
		sbox = IntegerDigits[FromDigits[s, 16], 16, StringLength[s]];
		sbox[[x + 1]]
)]
