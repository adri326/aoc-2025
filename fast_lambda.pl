% Needed for the definition of the `\+`, `\` and `^` operators and to provide support for metacalling.
:- use_module(library(lambda)).

:- module(fast_lambda, []).
:- use_module(library(iso_ext)).

subst_lambda(LambdaLit, LambdaCall, LambdaName, LambdaLit) :-
    nonvar(LambdaLit),
    (LambdaLit = Free+\Body ; LambdaLit = \Body, Free = t),
    LambdaCall =.. [LambdaName, Free].

subst_lambda(Compound, SubstCompound, LambdaName, LambdaLit) :-
    compound(Compound),
    Compound =.. [Name|Args],
    subst_lambda_in(Args, SubstArgs, LambdaName, LambdaLit),
    SubstCompound =.. [Name|SubstArgs].

subst_lambda_in([Before | Rest], [After | Rest], LambdaName, LambdaLit) :-
    subst_lambda(Before, After, LambdaName, LambdaLit).
subst_lambda_in([X | Rest], [X | SubstRest], LambdaName, LambdaLit) :-
    subst_lambda_in(Rest, SubstRest, LambdaName, LambdaLit).

parse_lambda(Free+\ArgsAndBody, Free, Args, Body) :-
    parse_lambda_body(ArgsAndBody, Args, Body).
parse_lambda(\ArgsAndBody, t, Args, Body) :-
    parse_lambda_body(ArgsAndBody, Args, Body).

parse_lambda_body(Arg^LitRest, [Arg | Rest], Body) :-
    parse_lambda_body(LitRest, Rest, Body).
parse_lambda_body(Body, [], Body) :-
    Body \= _ ^ _.

:- dynamic(global_lambda_counter/1).
global_lambda_counter(0).

get_lambda_name(Prefix, Output) :-
    global_lambda_counter(N),
    N1 is N + 1,
    retract(global_lambda_counter(N)),
    assertz(global_lambda_counter(N1)),

    number_chars(N, NStr),
    atom_chars(NAtom, NStr),
    atom_concat(Prefix, '_lambda_', PrefixPlusLambda),
    atom_concat(PrefixPlusLambda, NAtom, Output).

% TODO: is the --> syntax support needed?
term_expansion((PredHeader :- PredBody), [(LambdaHeader :- LambdaBody), (PredHeader :- SubstPredBody)]) :-
    compound(PredHeader),
    PredHeader =.. [PredName | _],
    get_lambda_name(PredName, LambdaName),
    subst_lambda(PredBody, SubstPredBody, LambdaName, LambdaLit),
    parse_lambda(LambdaLit, LitFree, LitArgs, LitBody),
    !,
    % TODO: check if I need to use prolog_load_context/2
    copy_term_nat(LitFree+LitArgs+LitBody, Free+LambdaArgs+LambdaBody),
    LambdaHeader =.. [LambdaName, Free | LambdaArgs].
