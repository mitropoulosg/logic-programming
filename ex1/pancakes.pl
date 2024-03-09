del(X, [X|Tail], Tail).
del(X, [Y|Tail], [Y|Tail1]) :-
   del(X, Tail, Tail1).

permutation2([], []).
permutation2(L, [X|P]) :-
   del(X, L, L1),
   permutation2(L1, P).

sorted([]).     /* if a list is sorted */
sorted([_]).
sorted([X,Y|Rest]) :-
   X =< Y,
   sorted([Y|Rest]).

allbetween(L,U,X):-
   findall(Y,between(L,U,Y),X).

between(L, U, L) :- L =< U.
between(L, U, X) :- L < U, L1 is L+1, between(L1, U, X).


initial(State):-
   permutation2(State, L1),
   length(State,N),
   allbetween(1,N,L2),
   L1 = L2.


final_state(State) :- 
   sorted(State).

pancakes_dfs(State,Operators,States) :-
   initial(State),
   dfs(State, [State],[], Operators,States).

dfs(State, States,Operators,Operators, States) :-
   final_state(State).
   
dfs(State1, SoFarStates,SoFarOperators,Operators,States) :-
   move(State1, State2, Operator),
   \+ member(State2, SoFarStates),
   append(SoFarOperators, [Operator], NewSoFarOperators),
   append(SoFarStates, [State2], NewSoFarStates),
   dfs(State2, NewSoFarStates, NewSoFarOperators,Operators, States). 

move(State1, State2,Operator) :-
   length(State1, N1),              % Number of pancakes 
   between(1, N1, Operator),               % Select a pancake to reverse
                                               %  the whole stack above it                      

   State1 \= [Operator|_],         /* This should not be the top pancake */
   append(Prefix, [Operator|Rest], State1),    % Isolate pancakes above
                                               % the one acting as operator                                                 
   reverse(Prefix, RevPrefix),                           % Reverse them 
   append([Operator|RevPrefix], Rest, State2).  % Build the final stack  


% this is for bonus part 

pancakes_ids(State,Operators,States) :-
   pancakes_idfs(State, Operators,States,0).

pancakes_idfs(State, Operators,States,Lim):-
   ldfs(State, [State],[], Operators,States,Lim).

pancakes_idfs(State, Operators,States,Lim) :-
   \+ ldfs(State,[State],[],Operators,States,Lim),
   Lim1 is Lim+1,
   pancakes_idfs(State, Operators,States,Lim1).

ldfs(State, States,Operators,Operators, States,_) :-
   final_state(State).
   
ldfs(State1, SoFarStates,SoFarOperators,Operators,States,Lim) :-
   Lim > 0,
   Lim1 is Lim - 1,
   move(State1, State2, Operator),
   \+ member(State2, SoFarStates),
   append(SoFarOperators, [Operator], NewSoFarOperators),
   append(SoFarStates, [State2], NewSoFarStates),
   ldfs(State2, NewSoFarStates, NewSoFarOperators,Operators, States,Lim1). 