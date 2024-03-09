%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Backtracking method (bt)                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bt_nqueens(N, Queens) :-
   make_tmpl(1, N, Queens),
   solution(N, Queens).

make_tmpl(N, N, [N/_]).
make_tmpl(I, N, [I/_|Rest]) :-
   I < N,
   I1 is I+1,
   make_tmpl(I1, N, Rest).

solution(_, []).
solution(N, [X/Y|Others]) :-
   solution(N, Others),
   between(1, N, Y),
   noattack(X/Y, Others).

between(I, J, I) :-
   I =< J.
between(I, J, X) :-
   I < J,
   I1 is I+1,
   between(I1, J, X).

noattack(_, []).
noattack(X/Y, [X1/Y1|Others]) :-
   Y =\= Y1,
   Y1-Y =\= X1-X,
   Y1-Y =\= X-X1,
   noattack(X/Y, Others).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Generate-and-test method (gt)                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gt_nqueens(N, Queens) :-
   choices(1, N, Choices),
   permutation(Choices, Queens),
   safe(Queens).

choices(N, N, [N]).
choices(M, N, [M|Ns]) :-
   M < N,
   M1 is M+1,
   choices(M1, N, Ns).

permutation([], []).
permutation([Head|Tail], PermList) :-
   permutation(Tail, PermTail),
   delete(Head, PermList, PermTail).

safe([]).
safe([Queens|Others]) :-
   safe(Others),
   noatt(Queens, Others, 1).

noatt(_, [], _).
noatt(Y, [Y1|Ylist], Xdist) :-
   Y1-Y =\= Xdist,
   Y-Y1 =\= Xdist,
   Dist1 is Xdist+1,
   noatt(Y, Ylist, Dist1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Diagonal lookahead method (dl)                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dl_nqueens(N, Queens) :-
   choices(1, N, Dxy),
   Nu1 is 1-N,
   Nu2 is N-1,
   choices(Nu1, Nu2, Du),
   Nv2 is 2*N,
   choices(2, Nv2, Dv),
   sol(Queens, Dxy, Dxy, Du, Dv).

sol([], [], _, _, _).
sol([Y|Ylist], [X|Dx1], Dy, Du, Dv) :-
   delete(Y, Dy, Dy1),
   U is X-Y,
   delete(U, Du, Du1),
   V is X+Y,
   delete(V, Dv, Dv1),
   sol(Ylist, Dx1, Dy1, Du1, Dv1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Forward checking method (fc)                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fc_nqueens(N, Queens) :-
   length(Queens, N),
   make_domain(N, Domain),
   combine_soldom(Queens, 1, Domain, SolDom),
   generate_solution(SolDom).

make_domain(1, [1]).
make_domain(N, Domain) :-
   N > 1,
   N1 is N - 1,
   make_domain(N1, RestDomain),
   append(RestDomain, [N], Domain).

combine_soldom([], _, _, []).
combine_soldom([X|Queens], I, Domain, [X-I-Domain|SolDom]) :-
   I1 is I + 1,
   combine_soldom(Queens, I1, Domain, SolDom).

generate_solution([]).
generate_solution([X-I-Domain|SolDom1]) :-
   member(X, Domain),
   update_domains(X, I, SolDom1, SolDom2),
   generate_solution(SolDom2).

update_domains(_, _, [], []).
update_domains(X, I1, [Y-I2-Domain1|SolDom1], [Y-I2-Domain2|SolDom2]) :-
   update_domain(X, I1, I2, Domain1, Domain2),
   update_domains(X, I1, SolDom1, SolDom2).

update_domain(X, I1, I2, Domain1, Domain4) :-
   remove_if_exists(X, Domain1, Domain2),
   XPlus is X + I2 - I1,
   remove_if_exists(XPlus, Domain2, Domain3),
   XMinus is X - I2 + I1,
   remove_if_exists(XMinus, Domain3, Domain4).

remove_if_exists(_, [], []).
remove_if_exists(X, [X|List], List) :-
   !.
remove_if_exists(X, [Y|List1], [Y|List2]) :-
   remove_if_exists(X, List1, List2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Forward checking method with MRV heuristic (fm)                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fm_nqueens(N, Queens) :-
   length(Queens, N),
   make_domain(N, Domain),
   combine_soldom(Queens, 1, Domain, SolDom),
   print([Soldom]),print(' SolDom \n'),
   generate_solution_with_fc_mrv(SolDom).

generate_solution_with_fc_mrv([]).
generate_solution_with_fc_mrv(SolDom1) :-

   mrv_var(SolDom1, X-I-Domain, SolDom2),

   member(X, Domain),
   update_domains(X, I, SolDom2, SolDom3),
   generate_solution_with_fc_mrv(SolDom3).

mrv_var([X-I-Domain], X-I-Domain, []).


mrv_var([X1-I1-Domain1|SolDom1], X-I-Domain, SolDom3) :-
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go(N, Method) :-
   cputime(T1),
   getall(N, Method, Sols),
   cputime(T2),
   length(Sols, L),
   T is T2-T1,
   write('There are '),
   write(L),
   writeln(' solutions.'),
   write('Time: '),
   write(T),
   writeln(' secs.').

getall(N, bt, Sols) :-
   findall(Sol, bt_nqueens(N, Sol), Sols).
getall(N, gt, Sols) :-
   findall(Sol, gt_nqueens(N, Sol), Sols).
getall(N, dl, Sols) :-
   findall(Sol, dl_nqueens(N, Sol), Sols).
getall(N, fc, Sols) :-
   findall(Sol, fc_nqueens(N, Sol), Sols).
getall(N, fm, Sols) :-
   findall(Sol, fm_nqueens(N, Sol), Sols).

go_all :-
   member(Method, [bt, gt, dl]),
   member(N, [6, 7, 8, 9, 10, 11]),
   nl,
   write('-----------------------'),
   nl,
   write('Method: '),
   write(Method),
   write('  Queens: '),
   write(N),
   nl,
   write('-----------------------'),
   nl,
   go(N, Method),
   fail.

go_all_fast :-
   member(Method, [dl, fc, fm]),
   member(N, [10, 11, 12, 13, 14]),
   nl,
   write('-----------------------'),
   nl,
   write('Method: '),
   write(Method),
   write('  Queens: '),
   write(N),
   nl,
   write('-----------------------'),
   nl,
   go(N, Method),
   fail. 