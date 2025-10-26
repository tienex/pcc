# Prolog Standards Support

This document provides comprehensive documentation of all Prolog standards supported by the PCC Prolog Compiler.

## Supported Standards (Chronological Order)

### 1. Edinburgh Prolog (1977)
**Status**: Fully Supported
**Command**: `plogcom -std edinburgh input.pl`

The original Prolog standard that established core syntax and semantics.

**Key Features**:
- Basic unification and backtracking
- Cut (!) for pruning search
- Negation by failure (\+)
- If-then-else (->; )
- DEC-10 style I/O predicates

**Example**:
```prolog
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).
```

### 2. DEC-10 Prolog (1970s)
**Status**: Fully Supported
**Command**: `plogcom -std dec10 input.pl`

Early influential implementation from Digital Equipment Corporation.

**Additional Features beyond Edinburgh**:
- `numbervars/3` - Number variables for printing
- `copy_term/2` - Deep copy of terms
- Enhanced I/O predicates

### 3. ISO Prolog (1995) - ISO/IEC 13211-1:1995
**Status**: Fully Compliant
**Command**: `plogcom -std iso input.pl` or `plogcom input.pl`

The official international standard for Prolog.

**Complete Standard Sections**:

#### 8.2 Term Unification
- `=/2` - Unification
- `\=/2` - Not unifiable

#### 8.3 Type Testing
- `var/1`, `nonvar/1` - Variable testing
- `atom/1`, `number/1`, `integer/1`, `float/1`
- `atomic/1`, `compound/1`

#### 8.4 Term Comparison
- `@</2`, `@=</2`, `@>/2`, `@>=/2` - Standard order
- `==/2`, `\==/2` - Structural equality
- `compare/3` - Three-way comparison

#### 8.5 Term Creation and Decomposition
- `functor/3` - Get functor and arity
- `arg/3` - Get nth argument
- `=../2` - Univ (term ↔ list)
- `copy_term/2` - Deep copy
- `term_variables/2` - Extract variables

#### 8.6 Arithmetic Evaluation
- `is/2` - Arithmetic evaluation

#### 8.7 Arithmetic Comparison
- `=:=/2`, `=\=/2` - Arithmetic equality
- `</2`, `=</2`, `>/2`, `>=/2`

#### 8.8 Clause Retrieval
- `clause/2` - Retrieve clause
- `current_predicate/1` - Check existence

#### 8.9 Clause Creation and Destruction
- `asserta/1`, `assertz/1` - Assert
- `retract/1`, `abolish/1` - Retract

#### 8.10 All Solutions
- `findall/3` - Find all solutions
- `bagof/3` - With duplicates
- `setof/3` - Sorted, unique

#### 8.11-8.14 I/O Predicates
Complete stream-based I/O with character, byte, and term operations.

#### 9.1-9.4 Arithmetic Functions
All ISO arithmetic functions including trigonometric, bitwise, and rounding.

### 4. ISO Prolog Corrigendum 1 (2007)
**Status**: Fully Supported
**Command**: `plogcom -std iso-cor1 input.pl`

**Clarifications**:
- Character escape sequences
- Operator precedence rules
- Error condition specifications
- Arithmetic function edge cases

**New Predicates**:
- `subsumes_term/2` - Subsumption test
- `acyclic_term/1` - Test if term is acyclic

### 5. ISO Prolog Corrigendum 2 (2012)
**Status**: Fully Supported
**Command**: `plogcom -std iso-cor2 input.pl`

**Enhancements**:
- Unicode character support
- Cyclic term unification handling
- Additional arithmetic clarifications

**New Predicates**:
- `ground/1` - Test if term has no variables

### 6. GNU Prolog
**Status**: Fully Compatible
**Command**: `plogcom -g input.pl` or `plogcom -std gnu input.pl` (default)

**Base**: ISO 1995 + Corrigenda
**Primary Extension**: CLP(FD) - Constraint Logic Programming over Finite Domains

**CLP(FD) Features**:
```prolog
X #= 5,
Y #> 3,
Z #< 10,
all_different([A,B,C]),
label([A,B,C]).
```

### 7. SWI-Prolog (Modern)
**Status**: Extensive Support
**Command**: `plogcom -std swi input.pl`

**Base**: ISO 1995 + Corrigenda

**Major Extensions**:

#### Dicts (SWI 7.x+)
```prolog
Point = point{x:10, y:20},
X = Point.x,  % X = 10
put_dict(x, Point, 15, Point2).
```

#### Native Strings
```prolog
string_concat("hello", " ", "world", S),
string_length("hello", 5),
split_string("a,b,c", ",", "", [a,b,c]).
```

#### Threads
```prolog
thread_create(worker(Data), Id, []),
thread_send_message(Id, task),
thread_join(Id, Status).
```

#### Tabling (Memoization)
```prolog
:- table fib/2.
fib(N, F) :- ...  % Automatically memoized
```

#### Constraints
- CLP(FD) - Finite domains
- CLP(R) - Real numbers
- CLP(Q) - Rational numbers

#### Rational Numbers
```prolog
X is 1 rdiv 3,  % Exact rational arithmetic
rational(X).
```

#### Global Variables
```prolog
nb_setval(counter, 0),  % Non-backtrackable
b_setval(var, X).       % Backtrackable
```

### 8. SICStus Prolog
**Status**: Compatible
**Command**: `plogcom -std sicstus input.pl`

**Base**: ISO 1995 + Corrigenda
**Extensions**: CLP(FD), CLP(R), CLP(Q), threads, Unicode

### 9. YAP Prolog
**Status**: Compatible
**Command**: `plogcom -std yap input.pl`

**Base**: ISO 1995 + Corrigenda
**Extensions**: Tabling, CLP(FD), threads, attributed variables

### 10. Turbo Prolog
**Status**: Supported
**Command**: `plogcom -t input.pl` or `plogcom -std turbo input.pl`

**Unique Features**:

#### Domains (Type System)
```prolog
domains
  name = symbol
  age = integer
  person = person(name, age)
```

#### Typed Predicates
```prolog
predicates
  parent(name, name)
  age_of(name, age)
```

#### Structured Sections
```prolog
clauses
  parent(tom, bob).
  parent(bob, ann).

goal
  parent(X, Y), write(X), nl, fail.
```

## Feature Comparison Table

| Feature Category | Ed. | DEC-10 | ISO 95 | ISO Cor2 | GNU | SWI | SICStus | YAP | Turbo |
|-----------------|-----|--------|--------|----------|-----|-----|---------|-----|-------|
| **Core Logic** |
| Unification | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Backtracking | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Cut (!) | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Negation (\+) | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **ISO Features** |
| ISO Syntax | - | - | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | - |
| ISO Built-ins | - | - | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | - |
| Exceptions | - | - | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | - |
| Streams | - | - | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | - |
| **Constraints** |
| CLP(FD) | - | - | - | - | ✓ | ✓ | ✓ | ✓ | - |
| CLP(R) | - | - | - | - | - | ✓ | ✓ | - | - |
| CLP(Q) | - | - | - | - | - | ✓ | ✓ | - | - |
| CHR | - | - | - | - | - | ✓ | - | - | - |
| **Advanced** |
| Tabling | - | - | - | - | - | ✓ | - | ✓ | - |
| Threads | - | - | - | - | - | ✓ | ✓ | ✓ | - |
| Modules | - | - | Part 2 | Part 2 | ✓ | ✓ | ✓ | ✓ | - |
| **Modern** |
| Dicts | - | - | - | - | - | ✓ | - | - | - |
| Strings | - | - | - | - | - | ✓ | - | - | - |
| Rationals | - | - | - | - | - | ✓ | ✓ | ✓ | - |
| Unicode | - | - | - | ✓ | - | ✓ | ✓ | ✓ | - |
| **Turbo-specific** |
| Domains | - | - | - | - | - | - | - | - | ✓ |
| Typed Predicates | - | - | - | - | - | - | - | - | ✓ |

## Built-in Predicate Count by Standard

- **Edinburgh**: ~50 predicates
- **ISO 1995**: 150+ predicates
- **ISO Cor2**: 153+ predicates
- **GNU**: 180+ predicates (ISO + CLP(FD))
- **SWI**: 500+ predicates (full featured)
- **SICStus**: 400+ predicates
- **YAP**: 350+ predicates
- **Turbo**: 100+ predicates

## Standard Selection Best Practices

1. **For learning**: Start with ISO 1995
2. **For portability**: Use ISO Cor2
3. **For constraints**: Use GNU or SWI
4. **For performance**: Consider YAP with tabling
5. **For modern features**: Use SWI
6. **For legacy code**: Match the original dialect

## References

### Standardization Documents
- ISO/IEC 13211-1:1995 - Prolog Part 1: General core
- ISO/IEC 13211-1:1995/Cor.1:2007
- ISO/IEC 13211-1:1995/Cor.2:2012
- ISO/IEC 13211-2:2000 - Prolog Part 2: Modules

### Historical Papers
- Warren et al., "Prolog - the language and its implementation compared with Lisp" (1977)
- Clocksin & Mellish, "Programming in Prolog" (1981)

### Implementation Documentation
- SWI-Prolog Manual: https://www.swi-prolog.org/pldoc/
- GNU Prolog Manual: http://www.gprolog.org/manual/
- SICStus Manual: https://sicstus.sics.se/documentation.html
- YAP Manual: https://www.dcc.fc.up.pt/~vsc/Yap/
