/*
 * ISO Prolog Test Suite
 * ISO/IEC 13211-1:1995
 */

% 8.2 Term unification
test_unify :-
	X = Y,
	Y = 5,
	X =:= 5.

% 8.3 Type testing
test_types :-
	var(X),
	atom(foo),
	integer(42),
	float(3.14),
	atomic(foo),
	atomic(42),
	compound(f(x)),
	nonvar(42),
	number(42),
	number(3.14).

% 8.4 Term comparison
test_compare :-
	foo @< goo,
	1 @< 2,
	f(1) @< f(2),
	compare(Order, 1, 2),
	Order == (<).

% 8.5 Term creation and decomposition
test_term_ops :-
	functor(f(a,b), F, A),
	F == f,
	A == 2,
	arg(1, f(a,b), X),
	X == a,
	f(a,b) =.. [f,a,b],
	copy_term(f(X,X), f(Y,Z)),
	Y \== Z.

% 8.6 Arithmetic evaluation
test_arith_eval :-
	X is 2 + 3,
	X =:= 5,
	Y is 10 / 2,
	Y =:= 5.0,
	Z is 10 // 3,
	Z =:= 3,
	W is 10 mod 3,
	W =:= 1.

% 8.7 Arithmetic comparison
test_arith_compare :-
	2 + 3 =:= 5,
	4 =\= 5,
	3 < 5,
	3 =< 5,
	5 > 3,
	5 >= 3.

% 8.8 Clause retrieval (dynamic required)
:- dynamic fact/1.

fact(1).
fact(2).
fact(3).

test_clause :-
	clause(fact(X), true),
	member(X, [1,2,3]).

% 8.9 Clause creation and destruction
test_assert_retract :-
	asserta(temp(1)),
	assertz(temp(2)),
	retract(temp(1)),
	\+ temp(1),
	temp(2),
	retractall(temp(_)).

% 8.10 All solutions
test_findall :-
	findall(X, fact(X), L),
	L = [1,2,3].

% 8.11-8.14 I/O predicates (would require actual I/O)

% 8.15 Logic and control
test_control :-
	\+ fail,
	\+ \+ true,
	once((X=1; X=2)),
	X == 1.

% 8.16 Atomic term processing
test_atom_ops :-
	atom_length(hello, 5),
	atom_concat(hello, world, helloworld),
	sub_atom(hello, 1, 3, _, ell),
	atom_chars(hello, [h,e,l,l,o]),
	atom_codes(hello, [104,101,108,108,111]),
	char_code(h, 104),
	number_chars(123, ['1','2','3']),
	number_codes(123, [49,50,51]).

% 9.1-9.4 Arithmetic functions
test_arith_funcs :-
	X is abs(-5),
	X =:= 5,
	Y is sign(-10),
	Y =:= -1,
	Z is float(5),
	Z =:= 5.0,
	W is floor(3.7),
	W =:= 3,
	T is ceiling(3.2),
	T =:= 4,
	R is round(3.5),
	R =:= 4,
	S is truncate(3.9),
	S =:= 3.

test_trig :-
	X is sin(0.0),
	X =:= 0.0,
	Y is cos(0.0),
	Y =:= 1.0.

test_bitwise :-
	X is 5 /\ 3,
	X =:= 1,
	Y is 5 \/ 3,
	Y =:= 7,
	Z is \ 0,
	Z =:= -1,
	W is 8 >> 2,
	W =:= 2,
	V is 2 << 2,
	V =:= 8.

% Main test runner
run_all_tests :-
	test_unify,
	test_types,
	test_compare,
	test_term_ops,
	test_arith_eval,
	test_arith_compare,
	test_clause,
	test_assert_retract,
	test_findall,
	test_control,
	test_atom_ops,
	test_arith_funcs,
	test_trig,
	test_bitwise,
	writeln('All ISO 1995 tests passed!').
