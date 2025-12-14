:- module(parsing, [
    integer/3,
    digits/3
]).
:- use_module(library(dcgs)).
:- use_module(library(lists)).

digit(X) --> (
    {var(X)} -> [Xs], { atom_codes(Xs, [Xc]), X is Xc - 48, X >= 0, X =< 10}
    ; { X >= 0, X < 10, Xc is X + 48, atom_codes(Xs, [Xc]) }, [Xs]
).

digits([X|R]) --> digit(X), digits(R).
digits([X]) --> digit(X).
digits_to_integer_([], 0).
digits_to_integer_([D|R], X) :-
    digits_to_integer_(R, Y),
    X is Y * 10 + D.

digits_to_integer(Ds, X) :-
    reverse(Ds, Dr),
    digits_to_integer_(Dr, X).

positive_integer(X) --> (
    {var(X)} -> digits(Ds), { digits_to_integer(Ds, X) }
    ; { X >= 0, X < 10} -> digit(X)
    ; { X >= 0, Y is X mod 10, R is X // 10 }, integer(R), digit(Y)
).

integer(X) -->
    {var(X)} -> (
        "-", positive_integer(Y), {X is -Y}
        ; positive_integer(X)
    )
    ; { X >= 0 } -> positive_integer(X)
    ; { X < 0, Y is -X }, "-", positive_integer(Y).
