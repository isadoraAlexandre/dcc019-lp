/**************** LISTAS EM PROLOG ************************

Prolog usa uma representação simbólica de lista, a qual contém uma cabeça e uma cauda.

A notação usada para lista é:
             [H | T]
H é um elemento da lista, a cabeça, e T é uma lista ou a cauda.
A lista vazia é representada por [].
Prolog permite descrever os elementos da lista, separando-os por uma vírgula, sem a necessidade de usar o operador |.

São exemplos de lista em Prolog:
- [a,b,c]
- [a | [b,c]]       
- [a,b | [c]]
- []
- [a,b,c | []]
- [a, [b,c], [], X]

O padrão [H | T] é mais usado na especificação de predicados envolvendo lista e na unificação.
Realize as seguintes consultas e observe o resultado:
?- [H | T] = [a,b,c].
?- [H | T] = [].
?- [H1, H2, H3] [a,b,c].
?- [H1, H2] = [a,b,c].
?- [H1, H2 | T] = [a,b,c].

Com base nisto, podemos definir predicados para obter o cabeça da lista (primeiro elemento) e a cauda da lista:
*/

car([H | _], H). % predicado que relaciona uma lista, [H | _], a sua cabeça, H
cdr([_ | T], T). % predicado que relaciona uma lista, [_ | T], a sua cauda, T

/*
 A resolução SLD e unificação funciona da mesma forma usando lista.
Por exemplo, abaixo está a árvore de prova para as consultas car([a,b,c], X) e cdr([a,b,c], X).

                                             car([a | [b,c]], X)
                                                     |
car([a | [b,c]], X)                                  | {H₀ / a, _₀ / [b,c], X / a}
        =                                            |
car([H₀ | _₀], H₀)                                  true




cdr([a | [b,c]], X)                          cdr([a | [b,c]], X)
        =                                            |
cdr([_₀ | T₀], T₀)                                   | {T₀ / [b,c], _₀ / a, X / [b,c]}
                                                     |
                                                    true

Exexmplos de predicados usando listas:
*/

% member/2
member(H, [H | _]). % elemento é a cabeça da lista
member(H, [_ | T]) :- member(H, T). % Se não é a cabeça, então tem que estar na cauda

/* ************************************ EXERCÍCIO ***************************************
1- A consulta member(a, [a,a,a]) tem três resultados iguais, quando o ; é apertado para
obter mais respostas. No entanto, se um elemento é membro de uma lista, não há necessidade de
realizar backtracking e tentar outras possibildade.
Modifique a definição de member para eliminar estes retrocessos desnecessário de modo que a consulta
member(a, [a,a,a]) tenha apenas uma resposta.
*/


% concate/3
concate([], L, L). % O resultado de concatenar uma lista vazia com outra lista L é L
concate([H | T1], L, [H | T2]) :- concate(T1, L, T2).

% *********************************** EXERCÍCIO *************************************
% 2- Realize as seguintes consultas e construa a árvore de prova para cada uma delas
% a) concate([a,b], [c], X).
% b) concate(X, [c], [a,b,c,d]).
% c) concate([a,b], X, [a,b,c]).

% add/3
add(X, L, [X | L]).

% *********************************** EXERCÍCIO *************************************
% 3- Qual o resultado da consulta add(X, [b,c], [a,b,c])?

% rem/3
rem(H, [H | T], T).
rem(X, [H | T], [H | R]) :- rem(X, T, R).

% *********************************** EXERCÍCIO *************************************
% 4- A consuta rem(a, [a,b,a,a], L) tem várias possibilidade.
% Modifique o predicado rem/3 para que tenha apenas uma solução

% len/2
len([], 0).
len([_ | T], N1) :- len(T, N), N1 is N + 1.

/* *********************************** EXERCÍCIO *************************************
5- A ordem dos predicados len(T, N) e N1 is N + 1 na definição de len pode ser invertida? Por que?

6- Defina um predicado intercala(XS, Y S, ZS) de tal modo que ZS seja a intercalação entre os elementos de XS e YS.
O tamanho de ZS deve ser o mesmo tamanho da menor lista entre XS e YS.
Por exemplo, a consulta intercala([1, 2, 3, 4, 5], [a, b, c], XS) deve produzir como resultado XS = [1, a, 2, b, 3, c].

7- Defina um predicado prefix(XS, YS) tal que XS é um prefixo da lista YS.
Por exemplo, a consulta prefix(XS, [a, b, c, d]) produz como resultado:
- XS = [];
- XS = [a];
- XS = [a, b].

8- Defina a regra l2(XS) que resulta em sucesso se, e somente se, XS é uma lista com exatamente dois elementos.

*/


/*
Estruturas complexas podem ser facilmente representadas em Prolog usando estrutuas.
Por exemplo, podemos representar uma árvore binária da seguinte forma:
                   tree(value, esq, dir)

Ex.:
                              2
               ———————————————|————————————
              |                            |
              1                            4
                                     ——————|—————
                                    |            |
                                    3            5
             

*/
tree(2, tree(1, nil, nil),
        tree(4, tree(3, nil, nil),
	        tree(5, nil, nil))).

% Com base nesta definição, podemos escrever um predicado para realizar a busca binária em árvore:

busca(N, tree(N, _, _)). % elemento é a raíz da árvore
busca(N, tree(X, E, _)) :- N < X, !, busca(N, E). % elemento está na subárvore esquerda
busca(N, tree(_, _, D)) :- busca(N, D).

/* *********************************** EXERCÍCIO ************************************* */
% 9- Faça um predicado para inserção em listas
