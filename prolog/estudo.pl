primo(joao, jose).
primo(joao, julio).
primo(jaco, julio).
primo(jair, jorge).
prima(lucia, jose).
prima(marcia, jair).
prima(carla, joao).

%tem_primo(X) :- primo(X,Y), !, prima(Z, X).

max(X, Y, X) :- X > Y, !.
max(X, Y, Y).

pai(mufasa, simba).
pai(simba, kiara).
pai(simba, kopa).
mae(nala, kiara).
mae(sarabi, simba).
mae(nala, kopa).

progenitor(X, Y) :- mae(X, Y); pai(X, Y) , not(X=Y).

descendente(X, Y) :- progenitor(Y, X). 
descendente(X, Y) :- progenitor(Z, X), descendente(Z, Y).

proud(X) :- pai(X,Y) , newborn(Y),!.
pai(X,Y) :- pais(X,Y) ,men(X).
pais(john,mary) .
pais(john,chris) .
men(john) .
newborn(mary).
newborn(chris).

top(X,Y) :- p(X,Y).
top(X,X) :- s(X).
p(X,Y) :- q(X), !, r(Y).
p(X,Y) :- s(X), r(Y). 
true(X).
q(a).
q(b).
r(c).
r(d).
s(e).

membro(X, [X|T]).
membro(X, [H|T]) :- membro(X, T).

conc([], L, L).
conc([X|T1], L2, [X|T3]) :- conc(T1, L2, T3).

insere(X, L, [X|L]).

del(X,[X|T],T).
del(X,[Y|T1],[Y|T2]):-del(X,T1,T2).

inserir(X, Y, [X|Y]).
inserir2(X, [H|T], [X|[H|T]]).
    
ultimo([X], X).
ultimo([H|T], X):-ultimo(T, X).

remove(X, [X|T], T).
remove(X, [Y|T1], [Y|T2]):- remove(X, T1, T2).

lshift([H|T], L):-ultimo([H|T], X) , remove(X,[H|T], R) , inserir(X,R,L).

intercal(_, [], []).
intercal([], _, []).
intercal([X|T1], [Y|T2], [X|[Y|Z]]):- intercal(T1, T2, Z).

iord(X, [], [X]).
iord(X, [H|T], [X|[H|T]]) :- X =< H, !.
iord(X, [H|T], [H|R]) :- iord(X, T, R).

fib(0,0).
fib(1,1).
fib(N, F):- 
    N > 1,
    N1 is N-1,
    N2 is N-2 , 
    fib(N1, F1) , 
    fib(N2, F2) , 
    F is F1 + F2.

func(_,_,[],[]).
func(A,B,[H|T1],[H|T2]):- H >= A , H =< B, func(A,B,T1,T2) ,!.
func(A,B,[_|T1],T2):- func(A,B,T1,T2).

prefix([], _).
prefix([H|T], [H|R]):-prefix(R,T),!.

sum([], 0).
sum([H|T], R):- sum(T, R1), R is H+R1.

len([], 0).
len([H|T], L):- len(T, L1), L is L1+1.

membro(X, [X|T]).
membro(X, [H|T]):- membro(X, T).

concat([], Y, Y).
concat([X|T], Y, [X|R]):-concat(T, Y, R).

ultimo([X], X).
ultimo([H|T], U):-ultimo(T,U), !.

reverse([], []).
reverse([H|T], R):-reverse(T, R1), concat(R1, [H], R).

count(_, [], 0).
count(X,[X|R], N):- count(X,R, N1),N is 1+N1.
count(X, [H|T], N):- X \= H,count(X,T,N).

remove_first(_, [], []).
remove_first(X, [X|T], T) :- !.
remove_first(X, [H|T], [H|R]) :- X\=H, remove_first(X, T, R).

rall(_,[],[]).
rall(X, [X|T], R):- rall(X,T,R).
rall(X, [H|T], [H|R]):- X\=H, rall(X,T,R).

nelem(_, [], []).
nelem(0, _, []):-!.
nelem(N, [H|T], [H|R]):- N1 is N-1, nelem(N1, T, R).

rmnelem(_,[],[]).
rmnelem(0,L, L):- !.
rmnelem(N, [H|T], R):- N1 is N-1, rmnelem(N1, T, R).

pal(L):- reverse(L,L).

%nao funciona nenhum do 2
sublist([],_).
sublist([S|R], [S|T]):- sublist(R, T).
sublist([S|R], [H|T]):- S\=H, sublist(R, T).

sublist2(Sub, List) :-
    concat(_, rest, List),
    concat(Sub, _, rest).

perm([], []).
perm([],P).

zip([], [],[]).
zip([X|T], [Y|R], [(X,Y)|S]):-zip(T, R, S). 

fat(0,1).
fat(1,1).
fat(N, F):- N > 1, N1 is N-1, fat(N1, F1), F is F1 * N.

rotate(L, 0, L) :- !.
rotate([H|T], N, R) :-
    N1 is N - 1,
    concat(T, [H], L2),
    rotate(L2, N1, R).

insert_at(_, _, [], []).
insert_at(X, 0, L, [X|L]):-!.
insert_at(X, P, [H|T], [H|R]):- P1 is P-1, insert_at(X,P1,T,R).

divpar([], _, []).
divpar([], [], _).
divpar([H|T], [H|P], I):- H mod 2 =:= 0, divpar(T, P, I).
divpar([H|T], P, [H|I]):- H mod 2 =:= 1, divpar(T, P, I).
divpar([H|T], P, I):- divpar(T, P, I).
