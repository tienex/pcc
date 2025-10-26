/*
 * SWI-Prolog Extensions Test Suite
 * Tests modern Prolog features from SWI-Prolog
 */

% Dicts (SWI 7.x+)
test_dicts :-
	D = point{x:10, y:20},
	X = D.x,
	X == 10,
	put_dict(x, D, 15, D2),
	D2.x == 15.

% Strings (SWI native strings)
test_strings :-
	string("hello"),
	string_concat("hello", "world", "helloworld"),
	string_length("hello", 5),
	string_codes("hello", [104,101,108,108,111]),
	string_chars("hello", ['h','e','l','l','o']),
	atomics_to_string([hello, ' ', world], "hello world"),
	split_string("a,b,c", ",", "", [a,b,c]).

% List operations (SWI extended)
test_lists :-
	append([1,2], [3,4], [1,2,3,4]),
	length([1,2,3], 3),
	member(2, [1,2,3]),
	memberchk(2, [1,2,3]),
	reverse([1,2,3], [3,2,1]),
	sort([3,1,2], [1,2,3]),
	msort([3,1,2,1], [1,1,2,3]),
	nth0(1, [a,b,c], b),
	nth1(2, [a,b,c], b),
	last([1,2,3], 3),
	sumlist([1,2,3], 6),
	max_list([1,3,2], 3),
	min_list([1,3,2], 1),
	numlist(1, 5, [1,2,3,4,5]).

% Association lists (AVL trees)
test_assoc :-
	list_to_assoc([a-1, b-2, c-3], Assoc),
	get_assoc(b, Assoc, 2),
	put_assoc(d, Assoc, 4, Assoc2),
	get_assoc(d, Assoc2, 4).

% Option processing
test_options :-
	Options = [color(red), size(10), debug(true)],
	option(color(C), Options),
	C == red,
	option(missing(M), Options, default),
	M == default.

% Execution control
test_control :-
	catch(throw(error), Error, true),
	Error == error,
	ignore(fail),
	call_cleanup(true, true),
	setup_call_cleanup(true, true, true),
	forall(member(X, [1,2,3]), X > 0).

% Format (printf-like formatting)
test_format :-
	sformat(S, 'Hello ~w!', [world]),
	S == 'Hello world!',
	sformat(S2, '~d + ~d = ~d', [2,3,5]),
	S2 == '2 + 3 = 5'.

% Utilities
test_utils :-
	between(1, 5, 3),
	succ(4, 5),
	plus(2, 3, 5),
	divmod(10, 3, Q, R),
	Q == 3,
	R == 1,
	ground(foo(bar)),
	\+ ground(foo(X)),
	subsumes_term(f(X), f(a)),
	unifiable(f(X), f(a), [X=a]).

% Aggregation
test_aggregate :-
	aggregate_all(count, member(_, [1,2,3]), Count),
	Count == 3,
	aggregate_all(sum(X), member(X, [1,2,3]), Sum),
	Sum == 6,
	aggregate_all(max(X), member(X, [1,3,2]), Max),
	Max == 3.

% Global variables (non-backtrackable)
test_global :-
	nb_setval(counter, 0),
	nb_getval(counter, V),
	V == 0,
	nb_setval(counter, 1),
	nb_getval(counter, V2),
	V2 == 1.

% Backtrackable global variables
test_global_backtrack :-
	b_setval(var, 1),
	b_getval(var, 1),
	(b_setval(var, 2), fail ; true),
	b_getval(var, 1).  % Backtracked

% Rational numbers
test_rational :-
	X is rdiv(1, 3),
	rational(X),
	rationalize(0.333333, R),
	rational(R).

% Main test runner
run_all_tests :-
	test_dicts,
	test_strings,
	test_lists,
	test_assoc,
	test_options,
	test_control,
	test_format,
	test_utils,
	test_aggregate,
	test_global,
	test_global_backtrack,
	test_rational,
	writeln('All SWI-Prolog extension tests passed!').
