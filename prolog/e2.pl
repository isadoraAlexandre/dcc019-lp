/* ************* OBSERVAÇÃO PARA SUBMISSÃO DOS EXERCÍCIOS **************
Submeta o arquivo com as resposta em Prolog e imagens das unificações
e árvores feitas à mão, quando pertinientes.
************************************************************************ */

/*
A unificação é o processo de encontrar uma substituição de variáveis de tal formal
que quando a substiuição é aplicada a ambos os termos, temos o mesmo resultado.

 - Por exemplo, o unificador dos termos p(X,f(Y),a) e p(a,f(a),Y) é a substiuição {X = a, Y = a}

A unificação tem um papel crucial no mecanismo de Prolog, visto que é a base do processo de resolução SLD.
O símbolo = é o operador de unificação de Prolog e podemos usá-lo em consultas para encontrar o unificador de 2 termos:

 * teste as seguintes consulta e observe o resultado:
   ?- p(X,f(Y),a) = p(a,f(a),Y)

   ?- p(X,f(Y),a) = p(a,f(b),Y)

   ?- p(X,f(Y),a) = p(Z,f(b),a)

   ?- f(X) = f(f(X))
*/

/* ************** EXERCÍCIO  ******************

1- Encontre o unificador mais geral para os termos a seguir e, após, verifique sua resposta usando Prolog

a) p(A) e p(b)
b) p(A,g(A),X) e p(g(a),B,b)
c) f(X,h(Y),g(X)) e f(g(Y),h(c),g(Z))
d) f(X,h(X),g(X)) e f(h(a),W,g(Z))

**********************************************

- Considere o programa a seguinte em Prolog.

   program P                       clause #      */   
 
p(a).                              /* #1 */  
p(X) :- q(X), r(X).                /* #2 */  
p(X) :- u(X).                      /* #3 */  
 
q(X) :- s(X).                      /* #4 */  


r(a).                              /* #5 */  
r(b).                              /* #6 */  


s(a).                              /* #7 */  
s(b).                              /* #8 */  
s(c).                              /* #9 */  
 
u(d).                              /* #10 */  

% * Execute a consulta p(X) e observe o resultado (use o ; para obter todas as possíveis soluções)

/*

Prolog tem um modo de trace que mostrar o processo de unificação, substituição e resolução SLD.
  - Para ativar o modo de trace execute no interpretador de Prolog o comando:
    ?- trace.
        ************************ dependendo da versão, o comando debug pode ser usado ********************************

2- No modo de trace, execute a mesma consulta p(X) e observe o resultado


Obs.: Para desabilitar o modo de trace execute o comando:
    ?- notrace.
    ************************** pode ser que precise executar o comando nodebug para efetivamente sair ***************

p(a).                              /* #1 */  
p(X) :- q(X), r(X).                /* #2 */  
p(X) :- u(X).                      /* #3 */   
q(X) :- s(X).                      /* #4 */  
r(a).                              /* #5 */  
r(b).                              /* #6 */  
s(a).                              /* #7 */  
s(b).                              /* #8 */  
s(c).                              /* #9 */   
u(d).                              /* #10 */ 
A árvore de prova da execução completa da consulta p(X) é:

                                         p(X)
      _____________________________________|________________________________
      |                                    |#2                             |
      |#1(X=a)                             |                               |#3
      |                                q(X),r(X)                           |
     true                                  |                              u(x)
     X=a;                                  |#4                             |
                                           |                               |#10(X=d)
                                       s(X),r(X)                           |
                                           |                              true
                       ____________________|____________________          X=d.
                       |                   |                   |
                       |#7(X=a)            |#8(X=b)            |#9(X=c)
                       |                   |                   |
                      r(a)                r(b)                r(c)
                       |                   |                   |
                  _____|_____          ____|____           ____|____
                  |         |          |       |           |       |
                  |#5       |#6        |#5     |#6         |#5     |#6
                  |         |          |       |           |       |
                 true      fail       fail    true        fail    fail
                 X=a;                         X=b;
*/

% ************** EXERCÍCIO  ******************

% 2- Considere o seguite programa em Prolog:

m(a,b).
m(b,c).
m(c,e).

n(X,Y) :- m(X,Y).
n(X,Y) :- m(X,Z), m(Z,Y).

% a) Mostre a árvore de prova para este programa e a consulta n(a,e).
% b) Confirme a sua resposta usando o Prolog


/* A ordem das regras em um programa Prolog é importante, pois o processo de resolução SLD
tenta unificar com as regras seguindo a ordem em que aparecem no programa

O exercício a seguir ilustra este processo.

*/


% ******************************* EXERCÍCIO ********************************************

% 3- Considere o seguinte programa:
child(anne, bridget).
child(bridget, caroline).
child(caroline, donna).
child(donna, emily).

% trecho de código para a resolução do item a)
descend(X,Y) :- child(X,Y).
descend(X,Y) :- child(X,Z), descend(Z,Y).

% trecho alterado para a versão do item b)
/* Descomente este trecho e comente o anterior quando for resolver o item b)

descend(X,Y) :- child(X,Z), descend(Z,Y).
descend(X,Y) :- child(X,Y).

*/

% trecho alterado para a versão do item c)
/* Descomente este trecho e comente o anterior quando for resolver o item c)

descend(X,Y) :- descend(Z,Y), child(X,Z).
descend(X,Y) :- child(X,Y).

*/

/* a) Qual o primeiro resultado da consulta descend(X,Y)?
Faça a árvore de prova completa (todas as respostas) para esta consulta.

   b) Modifique o programa para inverte a ordem da definição do predicado descend(X,Y), de modo que a
definição do predicado, nas as linhas 131 e 132, ficará:

descend(X,Y) :- child(X,Z), descend(Z,Y).
descend(X,Y) :- child(X,Y).

Realize novamente a consulta descend(X,Y). Qual o primeiro resultado?
Faça a árvore de prova completa para esta nova versão.

   c) Modifique a versão anterior do programa para inverter a ordem dos predicados child e descend na cauda
da primeira regra de descend de modo que a nova definição será:

descend(X,Y) :- descend(Z,Y), child(X,Z).
descend(X,Y) :- child(X,Y).

Realize novamente a consulta descend(X,Y). Qual o resultado?
Faça a árvore de prova para esta nova versão.

*/

% Considere o seguinte programa

f_constructed(f(T,_), T).
bizarre(X) :- f_constructed(X,X).
crazy(X) :- bizarre(f(_,X)).

/* ******************************* EXERCÍCIO ********************************************

4- Qual o resultado da consulta crazy(X) em Prolog?
5- Considerando o algoritmo de unificação com o teste de ocorrência, Prolog retorna a
resposta esperada? Por quê?
*/
