:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module(parsing).
:- use_module(utils).

parse_problem([]) --> ("\n" ; []).
parse_problem([Line | R]) --> digits(Line), "\n", parse_problem(R).

read_problem(Ranges) :- phrase_from_file(parse_problem(Ranges), "./inputs/day3.txt"), !.

ne(A, B) :- A #\= B.

max_number(N, Input, MaxNum) :-
    N #> 0,
    N1 #= N - 1,
    length(Input, L),
    N #=< L,

    % Pick `Chosen`
    append(NotLast, LastDigits, Input),
    % The length of LastDigits must be checked after `append`, or else CLP(ℤ) will make the unification fail ad infinitum in length/2:
    length(LastDigits, N1),
    max_member(Chosen, NotLast),

    % Find the first instance of `Chosen`:
    append(Prev, [Chosen | Remaining], Input),
    maplist(ne(Chosen), Prev),
    max_number(N1, Remaining, Rec),

    % Add the digit to MaxNum:
    MaxNum #= Chosen * (10 ^ N1) + Rec.
max_number(0, _, 0).

part1(X) :-
    read_problem(Lines),
    maplist(max_number(2), Lines, Pairs),
    sum(Pairs, X).

part2(X) :-
    read_problem(Lines),
    maplist(max_number(12), Lines, Twelves),
    sum(Twelves, X).
