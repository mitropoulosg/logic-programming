
:-compile(randms).

:- lib(ic).
:- lib(branch_and_bound).

maxsat(NV, NC, D, F, S, M):-
    create_formula(NV, NC, D, F),

    length(S,NV),
    S #:: [0,1],
    clauses(S,F,0,Cost),
    bb_min(search(S, 0, occurence, indomain_middle, complete, []),Cost,bb_options{strategy:dichotomic, from:0}),
    M #= NC-Cost.

clauses(_, [], Cost, Cost).
clauses(S, [Clause|Rest], Cost1, Cost):-
    predicater(Clause, S, 0, Result),
    Is_zero#=(Result#=0),
    Cost2 #= Cost1 + Is_zero,
    clauses(S, Rest, Cost2, Cost).


predicater([], _, Result, Result):-!.
predicater([Element|Rest], S, Result, Result2):-
    New_Element #= eval(abs(Element)),
    nth(New_Element,S,Value),
    ((Element #<0 ,
    New_Value #= (neg Value#=1)
    );( New_Value #= Value
    )),

    Result1 #= ((Result or New_Value)#=1),
    predicater(Rest, S, Result1, Result2).

nth(1, [X|_], X).
nth(N, [_|T], X) :-
    N > 1,
    N1 is N - 1,
    nth(N1, T, X).