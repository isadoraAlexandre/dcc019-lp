% Primeiro Programa em Prolog

/**
Para executar programa em Prolog usamos um interpretador.
A versão usada do interpretador é o swi-prolog.

Para iniciar o interpretador, abra um terminal e execute o comando swipl.

******* Prolog tem três construções básicas: fatos, regras e consulta *******

*/

% ******************** FATOS ************************************

% Representam verdades incondicionais do mundo modelado
   % representado com predicados (estruturas) finalizadas com um ponto

% Conjunto de fatos representando a relação de parentesco (slide) 
  % relação parent(X,Y) indica que X é um dos pais de Y 

parent(pam, bob). % pam é um dos pais de bob

parent(tom, bob). % tom é um dos pais de bob
parent(tom, liz). % tom é um dos pais de liz

parent(bob, ann). % bob é um dos pais de ann
parent(bob, pat). % bob é um dos pais de pat

parent(pat, jim). % pat é um dos pais de jim.

/**
Há duas formas de carregar um programa:
 1- No interpreatador digitar o comando [file_name].
    Exemplo: ?- [e1].

 2- Iniciar o swi-prolog passando o arquivo do programa como argumento
    Exemplo: swipl e1.pl
*/

% ******************** CONSULTAS ************************************

% Usamos Prolog para realizar consultas a respeito do programa (base de dados)
  % podemos conusultar se bob é um dos pais de pat
   % ?- parent(bob, pat).

  % podemos cosultar quem são os pais de liz
   % ?- parent(X, liz).

  % podemos consultar quem é filho de bob
   % ?- parent(bob, X).

  % podemos consultar quem é um dos avós de jim
   % ?- parent(X,Y), parent(Y,jim).
     % observe o uso da conjunção, denotada pelo símbolo vírgula

% ******************** REGRAS ************************************

 % As regras são cláusulas com apenas um consequente
   % A1 ^ A2 ^ ... ^ An -> B é expresso em prolog da seguinte maneira:
     % B :- A1, A2, ..., An.
     % B é denominado de cabeça
     % A1, A2, ..., An é denomidado de cauda

 % Fatos são regras cuja cauda é vazia
 % Consulta são regras cuja cabeça é vazia

 % Exemplo de regra que especifica avós
   % granparent(X, Y) especifica que X é um dos avós de Y

granparent(X,Y) :- parent(X,Z), parent(Z,Y).

 % Algumas consultas usando o predicado granparent:
  % granparent(X, jim).
  % granparent(X,Y).


% ******************************* EXERCÍCIOS **********************************

/* 1- Represente, em Prolog, o seguinte conhecimento:

  * Butch é um assassino
  * Mia e Marsellus são casados
  * Zed está morto
  * Marsellus mata todos que dão a Mia uma massagem nos pés
  * Mia ama todos que são bons dançarinos
  * Jules come qualquer coisa que seja nutritiva ou saborosa

*/

assassino(butch).
casado(mia, marcellus).
morto(zed).
mata(X) :- massagem(mia, X).

ama(X) :- dancarino(X).
come(X) :- nutritivo(X) ; sabor(X).
    

% 2- Considere a seguinte base de dados na qual o predicado filho(X, Y) representa que X é filho de Y:

filho(joao, jose).
filho(ana, jose).
filho(maria, jose).
filho(jose,tiago).
filho(pedro, judas).
filho(gabriel, judas).
filho(judas, tiago).
filho(augusto, manuel).

/* Faça:

a) uma consulta para verificar se pedro é filho de jose;
filho(pedro, jose)

b) uma consulta que retorna filhos de jose;
filho(X, jose)

c) um predicado pai, de dois argumentos, de modo que pai(X, Y) represente o fato de que X é pai de Y;
*/
pai(X, Y) :- filho(Y, X).
/*
d) uma consulta, usando o predicado pai, para determinar quem é o pai de tiago;
pai(Tiago, _)

e) um predicado tio/2, de maneira que tio(X, Y) expresse que X é tio de Y (desconsidere o sexo);
*/
tio(X, Y) :- pai(W, X), pai(W, Z), pai(Z, Y), not(X=Z).
/*
f) um predicado primo/2, de maneira que primo(X, Y) represente o fato de que X é primo de Y;
*/
primo(X, Y) :- tio(Z, Y) , pai(Z, X).
/*
g) um predicado parente/2, de modo que parente(X, Y) represente o fato que X é parente de Y.
*/
parente(X, Y) :- tio(X, Y); primo(X, Y).


% Para fechar o interpretador, execute o comando:
  % ?- halt.

% Observação: pode ser necessário usar o prediado que verifica se dois termos são diferentes: \=
  % a \= b.
