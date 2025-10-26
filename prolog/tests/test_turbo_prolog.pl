% Test file for Turbo Prolog compatibility
% This file demonstrates Turbo Prolog style declarations and structure

domains
  name = symbol
  age = integer
  person = person(name, age)
  personlist = person*

predicates
  parent(name, name)
  age_of(name, age)
  grandparent(name, name)
  ancestor(name, name)
  older(name, name)

clauses
  % Facts about family relationships
  parent(tom, bob).
  parent(tom, liz).
  parent(bob, ann).
  parent(bob, pat).
  parent(pat, jim).

  % Facts about ages
  age_of(tom, 65).
  age_of(bob, 40).
  age_of(liz, 38).
  age_of(ann, 18).
  age_of(pat, 16).
  age_of(jim, 2).

  % Rules
  grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

  ancestor(X, Y) :- parent(X, Y).
  ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

  older(X, Y) :- age_of(X, AgeX), age_of(Y, AgeY), AgeX > AgeY.

goal
  grandparent(tom, X), write(X), nl, fail.
