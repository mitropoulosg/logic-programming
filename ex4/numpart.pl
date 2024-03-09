:- lib(ic).
:- lib(ic_global).

numpart(N,L1,L2):-
    is_even(N),

    findall(X, between(1, N, X), L),
    N1 #= eval(N/2),
    length(L1,N1),

    L1 #:: 1..N,

    ic : alldifferent(L1),
    append([1],_,L1),
    ordered_sum(L1, L1_Sum),
    L1_Sum #= eval(N * (N + 1) / 4),
    sum((L1*L1)) #= eval(N * (N + 1) * (2 * N + 1)/12),

    search(L1, 0, occurrence, indomain, complete, []),
    difference(L,L1,L2).


is_even(X) :-
    X #= 2 * _.

difference([], _, []).
difference([X|L1], L2, L3) :- 
    member(X, L2), !,
    difference(L1, L2, L3).
difference([X|L1], L2, [X|L3]) :- 
    difference(L1, L2, L3).

between(L, U, L) :- L =< U.
between(L, U, X) :- L < U, L1 is L+1, between(L1, U, X).
