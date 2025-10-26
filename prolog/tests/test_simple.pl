% Simple test file compatible with both Turbo Prolog and GNU Prolog
% Basic facts and rules

% Facts
likes(mary, food).
likes(mary, wine).
likes(john, wine).
likes(john, mary).

% Rules
happy(X) :- likes(X, wine).
happy(X) :- likes(X, food).

% List operations
first([H|_], H).
second([_,X|_], X).
last([X], X).
last([_|T], X) :- last(T, X).

% Simple arithmetic
double(X, Y) :- Y is X * 2.
square(X, Y) :- Y is X * X.

% Logic
and_test(X, Y) :- X, Y.
or_test(X, Y) :- X ; Y.
