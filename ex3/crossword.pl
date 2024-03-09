
my_findall(Template, Goal, List) :-         % a custom predicate for findall because findall didnt work as i wanted
    bagof(Template, Goal, List), !.
my_findall(_, _, []).
 
transpose([], []).          % a predicate to transpose a list
transpose([[]|_], []).
transpose(M, [Row|MT]) :-
    first_column(M, Row, Rest),
    transpose(Rest, MT).

first_column([], [], []).
first_column([[X|Xs]|M], [X|L], [Xs|Ls]) :-
    first_column(M, L, Ls).

crossword(SolvedList):-
    words(W),
    dimension(Dim),
    findall((X, Y), black(X, Y), Black),
    length(Black, N),
    N1 is (Dim * Dim) - N,
    length(L, N1),       % List L has length of the number of white spaces

    Dim1 is Dim + 1,
    make_cross(Black ,L, 1, 1, Dim1, Cross),        % this is the making of the cross
    make_domain(1,1,Cross,[],Horizontal,0, Dim1),       % this predicate takes every row of the cross and splits it to words
    transpose(Cross,Cross2),        % transpose it and do it for the vertical
    make_domain(1,1,Cross2,[],Vertical,1, Dim1),
    append(Horizontal,Vertical,Domains),         % append horizotan and vertical list
    findall(Names,(member(X,W),name(X,Names)),Words),       % here we store in Words all words that have to be used
    combine_soldom(Words,Domains,SolDom),       % SolDom is a list in form Word-Position_of_letters-Domains
    generate_solution(SolDom),!,

    findall(Name,(member(S-_-_,SolDom),name(Name,S)),SolvedList),
    print_cross(Cross),
    print('\n'),print('Solution = '),print(SolvedList),print('\n').

print_cross([]).
print_cross([Row|Cross]):-
    print_row(Row),print('\n'),
    print_cross(Cross).

print_row([]).
print_row([Element|Row]):-
    ((Element='###',print(Element));
    (Element\='###',name(X,[Element]),print(' '),print(X),print(' '))),
    print_row(Row).


generate_solution([]).
generate_solution(SolDom1) :-
    mrv_var(SolDom1, X-Pos-Domain, SolDom2),
    member(X, Domain),
    update_domains(X, Pos, SolDom2, SolDom3),   
    generate_solution(SolDom3).

mrv_var([X-I-Domain], X-I-Domain, []).      % exact the same predicate of Mr. Takis
mrv_var([X1-I1-Domain1|SolDom1], X-I-Domain, SolDom3) :-        % finds domain of least possible words
   mrv_var(SolDom1, X2-I2-Domain2, SolDom2),

   length(Domain1, N1),
   length(Domain2, N2),
   (N1 < N2 ->
      (X = X1,
       I = I1,
       Domain = Domain1,
       SolDom3 = SolDom1) ;
      (X = X2,
       I = I2,
       Domain = Domain2,
       SolDom3 = [X1-I1-Domain1|SolDom2])).

update_domains(_, _, [], []).
update_domains(X, Pos1, [Y-Pos2-Domain1|SolDom1], [Y-Pos2-Domain2|SolDom2]) :-          % for every other word remove some word of its domain
   update_domain(X, Pos1, Pos2, Domain1, Domain2),
   update_domains(X, Pos1, SolDom1, SolDom2).

update_domain(X, Pos1, Pos2, Domain1, Domain3) :-
    remove_if_exists(X, Domain1, Domain2),          % remove the same word from the domain of y word, that X word got 
    remove_if_exists2( X, Pos1, Pos2, Domain2,Domain3).  

 remove_if_exists2( X, Pos1, Pos2, Domain2,Domain3):- 
    (member(P,Pos1),member(P,Pos2),         % if x word and y word have a letter in common
    find_letter(X,Pos1,P,Letter),  % find this letter
    find_pos(Pos2,P,Position)->    % find its position
    findall(K,(member(K,Domain2), nth(Position,K,Letter)),Domain3);  % domain3 is a list that has all words from domain2 that have this letter in this position 
    Domain3=Domain2).  % else

find_pos([X|_], X, 0):-!.
find_pos([_|Y], Letter, Position):-
 find_pos(Y,Letter,Position1),
 Position is Position1 + 1.   

find_letter([Y|_],[PosX|_],PosX,Y):-!.
find_letter([_|X],[_|Pos1],Pos,Letter1):-
    find_letter(X,Pos1,Pos,Letter1).


remove_if_exists(_, [], []).
remove_if_exists(X, [X|List], List) :-
   !.
remove_if_exists(X, [Y|List1], [Y|List2]) :-
   remove_if_exists(X, List1, List2).


combine_soldom(_, [], []).
combine_soldom(Words,[Word-Pos |Domains],[Word-Pos-Dom|List]):-
    findall(X,(member(X,Words),length(X)=length(Word)),Dom),        % domains are all the possible words for the word Word
    combine_soldom(Words,Domains,List).

make_domain(Dim, _, [], List2, List2, _, Dim).
make_domain(X, Y, [Line | Cross], SoFar ,List2, Type, Dim):-
    make_words(Line, X, Y,[],[], List, Type, Dim),      % for every line return lists seperated with ###
    Y1 is 1,
    X1 is X + 1,
    X=< Dim - 1,
    my_findall(Element-Pos,(member(Element-Pos,List), length(Element)> 1), NewList),    % for length> 1
    append(SoFar, NewList, NewSoFar),
    make_domain(X1, Y1, Cross, NewSoFar ,List2, Type, Dim).
    
make_words([], _, Dim, _, _, [], _, Dim).
make_words([Element | Line], X, Y, SoFar,SoFarPos, [List1-Pos|List2], Type, Dim):-
    \+ var(Element), Y1 is Y + 1, Y =< Dim - 1,         % if it is black
    (( Y = 1, make_words( Line, X, Y1 ,[] , [], [List1-Pos|List2], Type, Dim));         % for first repetition
    (Y \= 1, List1 = SoFar, Pos = SoFarPos, make_words( Line, X, Y1,[] , [], List2, Type, Dim)));       % append so far list if found black

    var(Element),       % if the element is a variable so it is not black
    Y1 is Y + 1,
    append(SoFar, [Element], NewSoFar), % append it

    (Type = 0, append(SoFarPos, [X-Y], NewSoFarPos);        % given type 0 is for horizontal store, position of element on the table
    (Type = 1, append(SoFarPos, [Y-X], NewSoFarPos))),      % given type 1 is for horizontal store,

    Dim1 is Dim-1,
    ((Y < Dim1,
    make_words( Line, X, Y1,NewSoFar ,  NewSoFarPos, [List1-Pos|List2], Type, Dim));
    (Y = Dim1,List1 = NewSoFar, Pos = NewSoFarPos,          % if it is last repetion append so far list
    make_words( Line, X, Y1,NewSoFar , NewSoFarPos, List2, Type, Dim))).


make_cross(_, [], Dim, _, Dim, []).
make_cross(Black, L, X, Y, Dim, [List | List2]):-
    make_row(Dim, Black, L, X, Y, [], SoFarL, List),        % for every row
    Y1 is 1,
    X1 is X + 1,
    X=< Dim - 1,
    make_cross(Black, SoFarL, X1, Y1, Dim, List2).

make_row(Dim, _, [], _, Y, SoFar, [], List):-       % this is for the case that all white spaces have ended so the list is [] and there are some black spaces left
    append(SoFar, ['###'], NewSoFar),
    Y1 is Y + 1,
    Y =< Dim - 1,
    make_row(Dim, _, [], _, Y1, NewSoFar, _, List).

make_row(Dim, _, L, _, Dim, SoFar, L, SoFar).
make_row(Dim, Black, [Curr | L], X, Y, SoFar, SoFarL, List):-
    (\+member((X, Y), Black),       % if current element is not black
    Y1 is Y + 1,
    Y =< Dim - 1,
    append(SoFar, [Curr], NewSoFar), %append the element
    make_row(Dim, Black, L, X, Y1, NewSoFar, SoFarL, List));
    (member((X, Y), Black),         % if it is black
    Y1 is Y + 1,
    Y =< Dim - 1,
    append(SoFar, ['###'], NewSoFar)),
    make_row(Dim, Black, [Curr | L], X, Y1, NewSoFar, SoFarL, List). 


nth(0, [X|_], X).
nth(N, [_|Xs], X) :-
    N > 0,
    N1 is N - 1,
    nth(N1, Xs, X).

