:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(parsing).
:- use_module(utils).

parse_range(From-To) --> integer(From), "-", integer(To), "\n".

parse_ranges([]) --> ("\n" ; []).
parse_ranges([R | Rs]) --> parse_range(R), parse_ranges(Rs).

parse_ingredients([]) --> ("\n" ; []).
parse_ingredients([I | Is]) --> integer(I), "\n", parse_ingredients(Is).

parse_problem(Rs, Is) --> parse_ranges(Rs), parse_ingredients(Is).
read_problem(Rs, Is) :-
    phrase_from_file(parse_problem(Rs, Is), "./inputs/day5.txt"),
    !.

is_fresh([From-To | _], I) :- I =< To, I >= From, !.
is_fresh([_ | R], I) :- is_fresh(R, I).

part1(N) :-
    read_problem(Rs, Is),
    part1(N, Rs, Is).

part1(0, _, []).
part1(X, Rs, [I | Is]) :-
    part1(Y, Rs, Is),
    (
        is_fresh(Rs, I) -> X is Y + 1
        ; X = Y
    ).

part2(N) :-
    read_problem(Rs, _),
    part2(N, Rs).
part2(N, Rs) :-
    ranges_to_domain(Rs, Dom),
    X in Dom,
    fd_size(X, N).
