
:- lib(gfd).
:- lib(gfd_search).

skyscr(PuzzleId, Template):-
    puzzle(PuzzleId, Dim, Left, Right, Up, Down, Template),

    reverse_table(Template, Reversed),      % reversing the template
    transpose(Template,Template2),          % transpose it
    reverse_table(Template2, Reversed2),    % reverse the transposed table

    func(Dim, Left, Template),
    func(Dim, Right, Reversed),
    func(Dim, Up, Template2),
    func(Dim, Down, Reversed2),
    
    gfd_search : search(Template,0,most_constrained,indomain,complete,[]),!.


func(_, [], []).
func(Dim, [View|Next], [List|Rest]):-
    List #:: 1..Dim,
    alldifferent(List),
    (View#=0 ;   % if there view is 0 then do nothing
    (View#\=0,
    max_list(List,List,Max_List), 
    nvalues(Max_List,(#=),N), % N is the number of diferent values in Max_list
    View#=N)), % the number of skyscrapes that can be seen (view) must be equal to N
    func(Dim, Next, Rest). 

max_list(_,[],[]).
max_list(List,[_|Rest],[Max|Max_List]):-
    append(Prev,Rest,List), % Max is the max of Prev which is so far List
    gfd : max(Prev,Max),
    max_list(List,Rest,Max_List).

transpose([], []).          % a predicate to transpose a list
transpose([[]|_], []).
transpose(M, [Row|MT]) :-
    first_column(M, Row, Rest),
    transpose(Rest, MT).

first_column([], [], []).
first_column([[X|Xs]|M], [X|L], [Xs|Ls]) :-
    first_column(M, L, Ls).


reverse_table(Table, ReversedTable) :-  % a predicate to reverse the list
    reverse_rows(Table, ReversedTable).

reverse_rows([], []).
reverse_rows([Row|Rest], [ReversedRow|ReversedRest]) :-
    reverse(Row, ReversedRow),
    reverse_rows(Rest, ReversedRest).





