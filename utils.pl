:- module(utils, [
    ranges_to_domain/2,
    sum/2
]).

ranges_to_domain([From-To], From..To).
ranges_to_domain([From-To | Rs], From..To \/ Ds) :-
    ranges_to_domain(Rs, Ds).

sum([], 0).
sum([X | Xs], S) :- sum(Xs, S1), S is X + S1.
