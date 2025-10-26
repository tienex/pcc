/*
 * Tabling (Memoization) Test Suite
 * SLG Resolution - XSB, SWI-Prolog, YAP
 */

% Basic tabling example
:- table fib/2.

fib(0, 0).
fib(1, 1).
fib(N, F) :-
	N > 1,
	N1 is N - 1,
	N2 is N - 2,
	fib(N1, F1),
	fib(N2, F2),
	F is F1 + F2.

test_fib_tabled :-
	fib(10, F),
	F == 55,
	fib(20, F20),
	F20 == 6765.

% Path finding with tabling (avoids infinite loops in cyclic graphs)
:- table path/2.

edge(a, b).
edge(b, c).
edge(c, d).
edge(d, e).
edge(c, a).  % Cycle: a->b->c->a

path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

test_path_tabled :-
	findall(P, path(a, P), Paths),
	length(Paths, L),
	L > 0.  % Without tabling, this would loop infinitely

% Reachability in a graph
:- table reach/2.

reach(X, Y) :- edge(X, Y).
reach(X, Y) :- reach(X, Z), edge(Z, Y).

test_reach :-
	reach(a, e),
	reach(a, a).  % Via cycle

% Shortest path with tabling and aggregation
:- table shortest_path/3.

shortest_path(X, Y, 1) :- edge(X, Y).
shortest_path(X, Y, D) :-
	edge(X, Z),
	shortest_path(Z, Y, D1),
	D is D1 + 1.

test_shortest_path :-
	shortest_path(a, e, D),
	D == 4.

% Well-founded semantics with tabling
:- table win/1.

% Game: node X is a winning position if there exists a move to a losing position
win(X) :- move(X, Y), \+ win(Y).

move(1, 2).
move(2, 3).
move(3, 1).  % Cycle

test_win :-
	% With tabling and well-founded semantics
	(win(1) ; \+ win(1)).

% Tabled negation (tnot)
:- table p/1.
:- table q/1.

p(1).
p(2).

q(X) :- p(X), tnot(r(X)).

r(1).

test_tnot :-
	q(2),
	\+ q(1).

% Incremental tabling (SWI-Prolog)
:- table inc_fact/1 as incremental.
:- dynamic base_fact/1 as incremental.

base_fact(1).
base_fact(2).

inc_fact(X) :- base_fact(X).
inc_fact(X) :- base_fact(Y), X is Y + 10.

test_incremental :-
	findall(X, inc_fact(X), L1),
	length(L1, Len1),
	assertz(base_fact(3)),
	reeval(inc_fact(_)),
	findall(X, inc_fact(X), L2),
	length(L2, Len2),
	Len2 > Len1.

% Answer subsumption
:- table max_path/2 as (po(max_path_order/2)).

max_path_order(path(_, D1), path(_, D2)) :- D1 >= D2.

max_path(X, path(Y, D)) :- shortest_path(X, Y, D).

test_answer_subsumption :-
	max_path(a, path(e, D)),
	D == 4.

% Lattice-based tabling (for abstract interpretation)
:- table analyze/2 as (lattice(analysis_lattice/3)).

analysis_lattice(bottom, _, bottom).
analysis_lattice(_, bottom, bottom).
analysis_lattice(A, B, C) :- join(A, B, C).

join(int, int, int).
join(int, float, number).
join(float, int, number).
join(float, float, float).

analyze(var(x), int).
analyze(var(y), float).
analyze(plus(X, Y), T) :-
	analyze(X, T1),
	analyze(Y, T2),
	join(T1, T2, T).

test_lattice :-
	analyze(plus(var(x), var(y)), number).

% Shared tabling (for multi-threaded access)
:- table shared_data/1 as shared.

shared_data(1).
shared_data(2).

test_shared :-
	findall(X, shared_data(X), [1,2]).

% Table inspection
test_table_statistics :-
	table_statistics(fib/2, Stats),
	memberchk(variant_table_space_used(_), Stats).

% Abolish tables
test_abolish_tables :-
	fib(5, _),  % Create table
	abolish_table_pred(fib/2),
	% Table is cleared
	fib(5, F),
	F == 5.

% Main test runner
run_all_tests :-
	test_fib_tabled,
	test_path_tabled,
	test_reach,
	test_shortest_path,
	test_win,
	test_tnot,
	test_incremental,
	test_answer_subsumption,
	test_lattice,
	test_shared,
	test_abolish_tables,
	writeln('All tabling tests passed!').
