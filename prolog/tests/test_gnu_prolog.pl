% Test file for GNU Prolog compatibility
% This file tests basic Prolog predicates compatible with GNU Prolog

% Facts
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Rules
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

% List operations
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

reverse([], []).
reverse([H|T], R) :- reverse(T, RT), append(RT, [H], R).

% Arithmetic
factorial(0, 1).
factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.

fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :- N > 1, N1 is N - 1, N2 is N - 2,
                   fibonacci(N1, F1), fibonacci(N2, F2), F is F1 + F2.

% Comparison predicates
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.

min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- X > Y.

% Type checking
is_list([]).
is_list([_|_]).

is_atom(X) :- atom(X).
is_number(X) :- number(X).

% Cut examples
max_cut(X, Y, X) :- X >= Y, !.
max_cut(_, Y, Y).

% Negation
not_member(_, []).
not_member(X, [H|T]) :- X \= H, not_member(X, T).

% Query examples (commented out - would be run interactively)
% ?- parent(tom, X).
% ?- grandparent(tom, X).
% ?- append([1,2], [3,4], X).
% ?- member(2, [1,2,3]).
% ?- factorial(5, F).
