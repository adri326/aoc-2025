:- use_module(library(pio)).
:- use_module(library(clpfd)).
:- [library(dcg/basics)].

parse_line(left-Num) --> "L", integer(Num), "\n".
parse_line(right-Num) --> "R", integer(Num), "\n".

parse_problem([]) --> ("\n" ; eos).
parse_problem([X | Xs]) --> parse_line(X), parse_problem(Xs).

read_problem(Xs) :- phrase_from_file(parse_problem(XsRev), "./inputs/day1.txt"),
    reverse(Xs, XsRev),
    !.

part1(N) :-
    read_problem(Xs),
    run_comb(_, Xs, S),
    count_zeroes(S, N).

count_zeroes([], 0).
count_zeroes([Nz | Rs], X) :-
    Nz \= 0,
    count_zeroes(Rs, X).
count_zeroes([0 | Rs], X) :-
    count_zeroes(Rs, X1),
    X is X1 + 1.

run_comb(50, [], [50]).
run_comb(X, [left-D | Rs], [X | Xs]) :-
    run_comb(Y, Rs, Xs),
    X is (Y - D) mod 100.
run_comb(X, [right-D | Rs], [X | Xs]) :-
    run_comb(Y, Rs, Xs),
    X is (Y + D) mod 100.

part2(N) :-
    read_problem(Xs),
    run_comb2(_, Xs, S),
    sum(S, N).

sum([], 0).
sum([X | Xs], S) :- sum(Xs, S1), S is X + S1.

run_comb2(50, [], []).
run_comb2(X, [left-D | Rs], [Zero | Zs]) :-
    run_comb2(Y, Rs, Zs),
    % Note that 100 + D - Y doesn't work when Y == 0, since we want to treat it as 100.
    Zero is (99 + D - ((Y - 1) mod 100)) // 100,
    X is (Y - D) mod 100.
run_comb2(X, [right-D | Rs], [Zero | Zs]) :-
    run_comb2(Y, Rs, Zs),
    Zero is (D + Y) // 100,
    X is (Y + D) mod 100.
