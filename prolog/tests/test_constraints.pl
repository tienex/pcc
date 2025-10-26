/*
 * Constraint Logic Programming Test Suite
 * CLP(FD), CLP(R), CLP(Q)
 */

% CLP(FD) - Finite Domain Constraints

% Basic domain constraints
test_basic_clpfd :-
	X #= 5,
	X == 5,
	Y #> 3,
	Y in 4..10,
	Z #< 10,
	Z in 1..9.

% Arithmetic constraints
test_arith_constraints :-
	X + Y #= 10,
	X #= 3,
	Y #= 7,
	A * B #= 12,
	A #= 3,
	B #= 4.

% Domain specification
test_domains :-
	X in 1..10,
	X #> 5,
	X #< 8,
	% X is now in {6,7}
	label([X]).

% All different constraint
test_all_different :-
	Vars = [X,Y,Z],
	Vars ins 1..3,
	all_different(Vars),
	label(Vars),
	% One solution: [1,2,3] or permutation
	length(Vars, 3).

% N-Queens problem (classic CLP(FD) example)
n_queens(N, Qs) :-
	length(Qs, N),
	Qs ins 1..N,
	safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
	safe_queens(Qs, Q, 1),
	safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
	Q0 #\= Q,
	abs(Q0 - Q) #\= D0,
	D1 #= D0 + 1,
	safe_queens(Qs, Q0, D1).

test_n_queens :-
	n_queens(4, Qs),
	label(Qs),
	length(Qs, 4).

% Sum constraint
test_sum :-
	sum([X,Y,Z], #=, 10),
	X in 1..5,
	Y in 1..5,
	Z in 1..5,
	label([X,Y,Z]),
	X + Y + Z =:= 10.

% Scalar product
test_scalar_product :-
	scalar_product([2,3], [X,Y], #=, 13),
	X in 1..5,
	Y in 1..5,
	label([X,Y]),
	2*X + 3*Y =:= 13.

% Element constraint
test_element :-
	List = [10,20,30,40],
	element(2, List, X),
	X == 20.

% Global cardinality constraint
test_global_cardinality :-
	Vars = [1,2,1,3,1],
	global_cardinality(Vars, [1-Count1, 2-Count2, 3-Count3]),
	Count1 == 3,
	Count2 == 1,
	Count3 == 1.

% Labeling with options
test_labeling_options :-
	Vars = [X,Y,Z],
	Vars ins 1..10,
	X + Y + Z #= 15,
	labeling([min(X)], Vars).  % Minimize X

% Reification (constraints as boolean)
test_reification :-
	B #<==> (X #> 5),
	X #= 3,
	B #= 0,  % False

	B2 #<==> (Y #> 5),
	Y #= 10,
	B2 #= 1.  % True

% CLP(R) - Constraints over Reals

test_clp_r :-
	{X > 0},
	{X < 10},
	{X = 5.5},
	X =:= 5.5.

test_clp_r_linear :-
	{2*X + 3*Y = 12},
	{X = 3},
	Y =:= 2.0.

test_minimize :-
	{X >= 0, Y >= 0},
	{X + Y >= 4},
	{2*X + Y >= 5},
	minimize(3*X + 4*Y).

% CLP(Q) - Constraints over Rationals

test_clp_q :-
	{X > 0},
	{X < 1},
	{X = 1/2},
	X =:= 0.5.

test_clp_q_rational :-
	{X + Y = 1},
	{X = 1 rdiv 3},
	{Y = 2 rdiv 3}.

% Cryptarithmetic puzzle: SEND + MORE = MONEY
send_more_money(Digits) :-
	Digits = [S,E,N,D,M,O,R,Y],
	Digits ins 0..9,
	all_different(Digits),
	S #> 0,
	M #> 0,
	       1000*S + 100*E + 10*N + D
	     + 1000*M + 100*O + 10*R + E
	#= 10000*M + 1000*O + 100*N + 10*E + Y,
	label(Digits).

test_send_more_money :-
	send_more_money([S,E,N,D,M,O,R,Y]),
	S == 9,
	E == 5,
	N == 6,
	D == 7,
	M == 1,
	O == 0,
	R == 8,
	Y == 2.

% Main test runner
run_all_tests :-
	test_basic_clpfd,
	test_arith_constraints,
	test_domains,
	test_all_different,
	test_n_queens,
	test_sum,
	test_scalar_product,
	test_element,
	test_global_cardinality,
	test_labeling_options,
	test_reification,
	test_clp_r,
	test_clp_r_linear,
	test_clp_q,
	test_clp_q_rational,
	test_send_more_money,
	writeln('All constraint programming tests passed!').
