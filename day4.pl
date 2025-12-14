:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(lambda)).
:- use_module(library(iso_ext)).
:- use_module(parsing).
:- use_module(utils).

parse_line([]) --> [].
parse_line([1 | R]) --> "@", parse_line(R).
parse_line([0 | R]) --> ".", parse_line(R).

parse_problem([]) --> ("\n" ; []).
parse_problem([Line | Rest]) --> parse_line(Line), "\n", parse_problem(Rest).

read_problem(Ranges) :- phrase_from_file(parse_problem(Ranges), "./inputs/day4.txt"), !.

between_list(X, X, [X]).
between_list(From, To, [From | Rest]) :-
    From #< To,
    F1 #= From + 1,
    between_list(F1, To, Rest).

unify(A, A).

get_xy(Grid, X, Y, Cell) :-
    length(Grid, Height),
    (
        (var(Y) ; Y >= 0, Y < Height) -> (
            nth0(Y, Grid, Row),
            length(Row, Width),
            (
                (var(X) ; X >= 0, X < Width) -> nth0(X, Row, Cell)
                ; Cell = 0
            )
        )
        ; Cell = 0
    ).

get_row_neighborhood(NSize, Grid, X, Y, Row) :-
    From #= X - NSize,
    To #= X + NSize,
    between_list(From, To, Xs),
    length(Xs, L),
    length(Ys, L),
    maplist(unify(Y), Ys),
    maplist(get_xy(Grid), Xs, Ys, Row).

get_neighborhood(NSize, Grid, X, Y, Ngh) :-
    From #= Y - NSize,
    To #= Y + NSize,
    between_list(From, To, Ys),
    maplist(get_row_neighborhood(NSize, Grid, X), Ys, Ngh).

sum2([], 0).
sum2([Row | Rest], Sum) :-
    sum(Row, RowSum),
    sum2(Rest, RestSum),
    Sum is RowSum + RestSum.


grid(Grid, W, H) :-
    length(Grid, H),
    length(Ws, H),
    maplist(unify(W), Ws),
    maplist(length, Grid, Ws).

:- meta_predicate(forall_between(?, ?, 1)).
forall_between(From, To, Pred) :-
    between_list(From, To, List),
    maplist(Pred, List).

% Note: library(lambda) gets really slow when the closures get big
:- meta_predicate(map_xy(4, ?, ?)).
map_xy(Pred, FromGrid, ToGrid) :-
    grid(FromGrid, Width, Height),
    H1 is Height - 1,
    W1 is Width - 1,
    between_list(0, H1, Ys),
    between_list(0, W1, Xs),
    maplist(map_x(Pred, FromGrid, ToGrid, Xs), Ys).

:- meta_predicate(map_x(4, ?, ?, ?, ?)).
map_x(Pred, FromGrid, ToGrid, Xs, Y) :-
    maplist(map_(Pred, FromGrid, ToGrid, Y), Xs).

:- meta_predicate(map_(4, ?, ?, ?, ?)).
map_(Pred, FromGrid, ToGrid, Y, X) :-
    call(Pred, FromGrid, ToGrid, X, Y).

write_row([]) :- write('\n').
write_row([Cell | Rest]) :- write(Cell), write_row(Rest).
write_grid([]).
write_grid([Row | Rest]) :-
    write_row(Row),
    write_grid(Rest).

remove_crate(FromGrid, ToGrid, X, Y) :-
    get_xy(ToGrid, X, Y, ToCell),
    (
        get_xy(FromGrid, X, Y, 0) -> ToCell = 0
        ;
            get_neighborhood(1, FromGrid, X, Y, Ngh),
            sum2(Ngh, Occupied),
            (
                Occupied < 5 -> ToCell = 0
                ; ToCell = 1
            )
    ).

remove_crates(FromGrid, ToGrid) :-
    grid(FromGrid, W, H),
    grid(ToGrid, W, H),

    map_xy(remove_crate, FromGrid, ToGrid).

part1(X) :-
    read_problem(Grid),
    findall(X-Y, (get_xy(Grid, X, Y, 1), get_neighborhood(1, Grid, X, Y, Ngh), sum2(Ngh, Occupied), Occupied < 5), Solutions),
    length(Solutions, X).

part2(FinalGrid, FinalGrid) :-
    forall(get_xy(FinalGrid, X, Y, 1), (
        get_neighborhood(1, FinalGrid, X, Y, Ngh),
        sum2(Ngh, Occupied),
        Occupied >= 5
    )),

    !.

part2(FromGrid, FinalGrid) :-
    remove_crates(FromGrid, MidGrid),
    write_grid(MidGrid),
    part2(MidGrid, FinalGrid).

part2(X) :-
    read_problem(Grid),
    part2(Grid, Final),
    sum2(Final, FinalCount),
    sum2(Grid, StartingCount),
    X is StartingCount - FinalCount.
