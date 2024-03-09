


assignment(NP,MT,ASA,ASP) :-
    findall(X,activity(X,_),AIds),          % Gather all activities in list AIds
    assign(AIds, NP, MT, ASA),
    gather(NP,ASA,List),
    gather_time(NP,List,List1),
    reverse(List,List2),
    find_ASP(NP,List2,List1,List3),
    reverse(List3,ASP).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functions for ASP list  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% building ASP

find_ASP(0,_,_,[]).
find_ASP(P,List1,List2,[P-L-T|List]):-
    P1 is P-1,
    nth(P1,List1,L),
    nth(P1,List2,T),
    P>0,
    find_ASP(P1,List1,List2,List).

% returns a list of sum time per PId

gather_time(0,_,[]).
gather_time(P,List,[Sum|L2]):-
    P1 is P-1,
    nth(P1,List,X),
    length(X,Len),
    sumTime(Len,X,0,Sum),   
    P>0, 
    gather_time(P1,List,L2).

% returns sum time of PId

sumTime(0,_,SoFar,SoFar).
sumTime(Len1,X,SoFar,Sum):-
    Len2 is Len1-1,
    nth(Len2,X,Act),
    activity(Act,act(A,B)),
    NewSoFar is SoFar + (B-A),
    Len1>0,
    sumTime(Len2,X,NewSoFar,Sum).


% returns List with a list activities per PId

gather(0,_,[]).
gather(P,ASA,[L1|List]):-
    findall(X,member(X-P,ASA),L1),
    P1 is P-1,
    P>0,  
    gather(P1,ASA,List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functions for ASA list  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

assign([], _, _, []).
assign([AId|AIds], NP, MT,[AId-PId|ASA]) :-
    assign(AIds, NP, MT, ASA),

    findall(X,member(_-X,ASA),L1),  % gather in a list so far PIds of ASA
    ((L1 = [], Maxvalue is 1);    % this is to avoid duplicates assignments 
    (L1 = [_|_] ,max(L1,Max),(( Max<NP , Maxvalue is Max+1);(Max>=NP , Maxvalue is NP)))),

    between(1, Maxvalue, PId),             % Select a person PId for activity AId

    activity(AId, act(Ab, Ae)),
    findall(X,member(X-PId,ASA),APIds),    % Gather in list APIds so far activities of PId
    Time is (Ae-Ab),
    valid(Ab, Ae, MT,Time,APIds).          % Is current assignment consistent with previous ones?

valid(_, _, _, _, []).
valid(Ab1, Ae1, MT, SoFarTime, [APId|APIds]) :-
    activity(APId, act(Ab2, Ae2)),
    (Ab1-Ae2>=1; Ab2-Ae1>=1),
    NewSoFar is (SoFarTime + (Ae2-Ab2)),
    MT>=NewSoFar,
    valid(Ab1, Ae1, MT,NewSoFar,APIds).


% Definitions of possible auxiliary predicates 

between(L, U, L) :- L =< U.
between(L, U, X) :- L < U, L1 is L+1, between(L1, U, X).

nth(0, [X|_], X).
nth(N, [_|Xs], X) :-
    N > 0,
    N1 is N - 1,
    nth(N1, Xs, X).
