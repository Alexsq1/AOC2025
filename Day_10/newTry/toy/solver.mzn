int: row;
int: col;
array[1..row, 1..col] of int: A;
array[1..col] of int: s;
array[1..row] of var 0..max(s): coefs;

constraint forall (k in 1..row) (coefs[k] >= 0);
constraint forall (c in 1..col) 
		(sum (r in 1..row) (A[r,c] * coefs[r]) = s[c]);
solve minimize sum(k in 1..row) (coefs[k]);

output [show(coefs)]
