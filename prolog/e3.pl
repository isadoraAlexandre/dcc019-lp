/*
Retrocesso (backtracking) automático é uma das principais características de Prolog, porém pode
ser a razão de possíveis ineficiências no programa.
Considerando que, as vezes, Prolog desperdiça tempo explorando posibilidade que não levarão a
uma resposta, a lingaugem oferece um operador de corte (!) para controlar o backtracking.

Quando o operador de corte é ativado durante uma prova, seu efeito é eliminar todas as possíveis recursões
que poderiam existir envolvendo todos os predicados desde o momento da ativação do operador até
a cláusula geradora do operador.
*/

% ************************************ EXERCÍCIO ************************************************
% 1- Considere o seguinte programa:
p(1).
p(2) :- !.
p(3).

% Qual o resultado das seguintes consultas em Prolog?

% a) p(X)
% b) p(X),p(Y)
% c) p(X), !, p(Y)

% Apresente a árvore de prova para cada uma destas consultas


% Usando o operador de corte, podemos definir um predicado para a negação como a seguir.

not(P) :- call(P), !, fail.
not(_).

% Nesta definição são usados os predicados fail, que sempre causa uma falha, e call(P)
% que dar sucesso caso seja possível provar P.

% ************************************ EXERCÍCIO ************************************************

% 2- Considere o programa:

t(X) :- q(X), not(r(X)).
r(X) :- w(X), not(s(X)).
q(a).
q(b).
q(c).
s(a) :- t(a).
s(c).
w(a).
w(b).

% O que acontece com a consulta t(a)?

% 3- Considere o programa a seguir:

jealous(X,Y) :- loves(X,Z), loves(Y,Z).

loves(vincent, mia).
loves(marsellus, mia).

/*
Esta definição tem como resultado que Vicent tem ciúmes de si mesmo e idem para Marsellus.
Usando o predicado not, modifique a definição do predicado jealous de modo que a consulta
jealous(X,X) seja falsa.
*/

% **************** ARITMÉTICA EM PROLOG ************************************************

/*
 Programas lógicos são Turing completo. Isto significa que qualquer problema computável
pode ser expresso usando programação lógica.

MAS COMO EXPRESSAR ARITMÉTICA EM PROLOG?

Podemos usar a notação de Peano para representar número e definir predicados considerando esta notação.
Por exemplo, sendo zero representado pela constante 0 e o predicado s(X) representando o sucessor do
número X, a adição pode assim ser definida:
*/

plus(0,X,X).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

% Faça a consulta plus(s(s(0)), s(s(s(0))), Z) representando a soma de 2 com 3 e verifique que a resposta
% será 5 na notação de Peano, s(s(s(s(s(0))))).

/* ************************************ EXERCÍCIO ************************************************

4- Usando a notação de Peano descrita anteriormente, defina um predicado para multiplicação (times)


Apesar de ser possível construir qualquer programa que use operações aritméticas usando esta notação,
podemos elencar dois desafios:
  - notação é verbosa
  - ineficiência visto que as operações aritméticas são resolvida usando o processo de unificação
ao invés de usar instruções da máquina

Por estas questões, Prolog têm predicados ``build-in'' para expressar aritméticas:
   - o predicado =:= é usado para verificar se duas expressões são E-unificáveis
     * Exemplos:
       ?- 2 + 3 =:= 1 + 4
     Note que este predicado apenas diz se duas expressões tem o mesmo valor. Observe a diferenã pra o operador igual!
       ?- 2 + 3 = 1 + 4.
     Variáveis (não unificadas) não devem ser usadas com este predicado.
     Teste a seguinte consulta e veja o resultado:
       ?- X =:= 1 + 4.

   - o predicado is é a igualdade aritmética
     * Exemplos:
       ?- 8 is 2 + 6.
       ?- 5 is 2 + 3.
     No entanto, o operando à esquerda deve ser o valor reduzido da expressão.
     Realize as seguintes consultas e observe o resultado:
       ?- 2 + 3 is 1 + 4.
       ?- 2 + 6 is 8.
     Uma variável pode ser usada como operando no lado esquerdo para unificá-la com o resultado da expressão:
       ?- X is 2 + 6.
     Porém, uma variável não instanciada nunca deve ser usada no lado direito da operação.
     Observe o que acontece com a consulta com uma variável não unificada do lado direito ou uma unificada do lado esquerdo:
       ?- 8 is 2 + X.
       ?- X = 5, X is 2 + 6.

Tabela dos Operadores em Prolog

Notação matemática            Notação em Prolog
————————————————————————————————————————————————
x < y                            X < Y.
x ≤ y                            X =< Y.
x = y                            X =:= Y.
x ≠ y                            X =\= Y.
x ≥ y                            X >= Y.
x > y                            X > Y.


************************************ EXERCÍCIO ************************************************

5- Defina o predicado increment(X,Y) que é verdadeiro sempre que o valor de Y for exatamente
o sucessor de X (uma unidade a mais).
*/
