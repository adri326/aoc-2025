:- module(utils, [
    ranges_to_domain/2,
    sum/2,
    max_member/2
]).
:- use_module(library(clpz)).
:- use_module(library(lists)).

ranges_to_domain([From-To], From..To).
ranges_to_domain([From-To | Rs], From..To \/ Ds) :-
    ranges_to_domain(Rs, Ds).

sum([], 0).
sum([X | Xs], S) :- sum(Xs, S1), S is X + S1.

is_ge(Higher, Lower) :- Higher #>= Lower.
is_gt(Higher, Lower) :- Higher #> Lower.

% If two elements are equal, then unifies `Max` with the first one and only returns one answer.
max_member(Max, List) :-
    append(Prev, [Max | Succ], List),
    maplist(is_gt(Max), Prev),
    maplist(is_ge(Max), Succ).
