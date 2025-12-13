:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(between)).
:- use_module(parsing).
:- use_module(utils).

parse_problem([A-B]) --> integer(A), "-", integer(B), ("\n" ; []).
parse_problem([A-B | R]) -->
    integer(A), "-", integer(B),
    (",\n" ; ","),
    parse_problem(R).

read_problem(Ranges) :- phrase_from_file(parse_problem(Ranges), "./inputs/day2.txt"), !.

repetition_to_multiplier(WordLength, Repetitions, Multiplier) :-
    Repetitions > 1,
    R1 is Repetitions - 1,
    repetition_to_multiplier(WordLength, R1, M1),
    Multiplier is M1 * (10 ^ WordLength) + 1.

repetition_to_multiplier(_, 1, 1).

fake_in_domain(Domain, Fake, Repetitions) :-
    between(1, 20, WordLength),
    repetition_to_multiplier(WordLength, Repetitions, Multiplier),
    Fake in Domain,
    Fake #>= 10 ^ (Repetitions * WordLength - 1),
    Word #>= 0,
    Word #< 10 ^ WordLength,
    % Do not allow leading zeroes:
    Fake #= Word * Multiplier,
    labeling([bisect], [WordLength, Fake]).

fake_in_domain(Domain, Fake) :-
    between(2, 20, Reps),
    fake_in_domain(Domain, Fake, Reps).

part1(X) :-
    read_problem(Ranges),
    ranges_to_domain(Ranges, Domain),
    setof(X, fake_in_domain(Domain, X, 2), Xs),
    sum(Xs, X).

part2(X) :-
    read_problem(Ranges),
    ranges_to_domain(Ranges, Domain),
    setof(X, fake_in_domain(Domain, X), Xs),
    sum(Xs, X).
