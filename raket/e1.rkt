#lang racket

#|******************************************** Tipos Abstratos de Dados *****************************************

Abstração pode ser entendido como o processo intelectual em que um objeto de reflexão é isolado de fatores
que lhe estão associados na realidade.

O processo de abstração é fundamental em computação, pois a todo momento cientistas da computação precisam
abstrair o mundo / problemas para modelá-los e resolvê-los algoritmicamente.
Linguagens de programação oferecem suporte para abstração, os quais podem ser de:
   - processos;
   - dados

A abstração de processos é realizada com criação de funções.
A abstração de dados é realizada via definição de novos tipos de dados.

Quando se define novos tipos de dados é importante que o uso do novo tipo seja realizada sempre
por meio de uma interface, nunca se manipule diretamente a implementação. Ou seja, deve-se ter uma clara
separação entre Interface e Implementação. Para isto, a interface deve prover operações para:
    - criar elementos do tipo;
    - consulta / manipulação do tipo

Outra característica importante na definição de um tipo de dados é a ocultação de informação, provendo a
clara separação entre uso do tipo de dados X implementação.

Para ilustrar, suponhamos a definição de um tipo para os números naturais.
Especificamos as seguintes operações para criar e manipular o tipo (Interface) :
     - zero                  ; construtor (criar) o valor 0
     - (is-zero? n)          ; operação para testar se um número é zero
     - (suc n)               ; operação que retorna o sucessor de um dado número
     - (pred n)              ; operação que retorna o predecessor de um número n

Apenas usando a interface, devemos ser capazes de operar usando os números naturais.
Assim, podemos definir funções sob os números naturais como a soma:
|#

(require "nat.rkt")

(define (soma x y)
  (if (is-zero? x)                  ; testa se x é 0
      y                             ; se x é 0, o resultado da soma é y
      (suc (soma (pred x) y))))     ; caso contrário, o resultado é o sucessor da soma entre o predecessor de x e y

; Podemos usar a função soma

(is-zero? (soma zero (suc zero)))

#|

Quando falamos de Tipo Abstrato de Dados Abstrato (TAD), nos referimos a definições de tipos e suas operações com uma separação
clara entre interface e implementação.

Se uma linguagem de programação oferece suporte a TADs, ela prover mecanismos para ocultação de informação.
  - Linguagens como Java e C++ prover suporte a TADs por meio de definição de classes e os modificadores public, private e protected
  - C não oferece suporte nativo para ocultação de informação de struct, não provendo suporte a TAD
        * Nota: o programador consegue implementar ocultação de informação, mas não com uma feature nativa da linguagem!

Em Racket há o conceito de módulos, no qual podemos realizar definições de tipos e funções e exportar, para os usuários do módulo,
somente as operações desejadas (interface).
Tipicamente, um módulo em Racket é contido em um arquivo e o nome do módulo é o nome do arquivo.
Usando este mecanismo, definimos o tipo natural no módulo nat, contido no arquivo nat.rkt, e usamos a sua interface neste módulo.
Para usar as operações do módulo nat foi necessário importá-lo usando a definição (require "nat.rkt") contido neste arquivo!

Observe que no arquivo nat.rkt há duas formas de implementar números naturais.
Altere a implementação e observe que o código deste arquivo continua funcionado. Esta é a vantagem de TAD e separação entre
interface e implementação!

|#

#| ************************************************* O Tipo Environment *************************************************

Um tipo muito importante em Linguagens de Programação e em todo o restante do curso é o tipo de dados Environment (Ambiente).
Essencialmente, este tipo de dados é um dicionário (mapeamento) entre nomes e valores.
A interface do tipo contém as seguintes operações:

   - (empty-env)                  =======> construtor de ambiente vazio 
   - (extend-env var value env)   =======> construtor de ambiente, via adição uma nova chave (var value) ao ambiente env
   - (apply-env env var)          =======> obtém valor associado a chave var no ambiente env / é um operação do tipo observer

Exemplo de usos do tipo Environment:
|#

(require "env.rkt")

(define env (extend-env 'd 6     ; cria um ambiente a partir da inserção da chave ('d, 6) no ambiente abaixo
              (extend-env 'a 1   ; cria um ambiente com a chave ('a, 1) no ambiente vazio
                  empty-env)))   ; cria um ambiente vazio
                            
(apply-env env 'd) ; obtém o valor associado com a chave 'd no ambiente env ====> resultado 6
(apply-env env 'a) ; obtém o valor associado com a chave 'a no ambiente env ====> resultado 1
;(apply-env env 'c) ; obtém o valor associado com a chave 'c no ambiente env ====> resultado é um erro


#|
A implementação do tipo Environment pode ser feita de duas maneiras:
   - tipos de dados (estrutura)
   - procedural

Antes de discutir a implementação por tipo de dados, observaremos que um novo tipo é definido sempre com a seguinte estrutra:
   (extend-env k₁ v₁
      (extend-env k₂ v₂    
         (extend-env k₃ v₃
            ...
               (extend-env kₙ vₙ empty-env))))

Note que um ambiente é criado com uma sequência de operações extend-env e finalizado com o empty-env.
Podemos representar um ambiente usando uma lista estruturada, na qual um ambiente é uma lista com o primeiro elemento
sendo uma tag indicando que é um extensão de ambiente, seguido pela chave, valor e o ambiente estendido.
O ambiente vazio pode ser representado por uma lista vazia ou por uma tag indicando um ambiente vazio.
Desde modo, um valor de uma ambiente pode ser descrito pela seguinte gramática:

   Env → 'empty-env | ('extend-env Symbol RacketValue Env)

em que Symbol é um símbolo Racket e RacketValue representa um valor Racket.
Deste modo, o ambiente env do exemplo anterior seria representado por:

    env = ('extend-env 'd 6 ('extend-env 'a 1 'empty-env))


Usando essa estrutura, a implementação da interface de Environment fica:
   * NOTA: para não conflitar com a definição no módulo env.rkt, inseri o prefixo v1
           indicando que é a primeira forma de implementação do ambiente
|#

; Definição do Environment usando Lista Estruturada
(define v1:empty-env
  'empty-env)

(define (v1:extend-env var value env)
  (list 'extend-env var value env))
 

(define (v1:apply-env env var)
  (match env
    ['empty-env (error "No bind")]
    [(list 'extend-env k v e)
     (if (eq? k var) v
         (v1:apply-env e var))]))
  
(define env1 (v1:extend-env 'd 6    
              (v1:extend-env 'a 1
                  v1:empty-env)))

(v1:apply-env env1 'd)
(v1:apply-env env1 'a)
;(v1:apply-env env1 'c)

#|

Uma outra maneira de implementar tipos de dados é usando registros.
Racket permite a criação de tipos de dados registro usando o diretiva struct.

A sintaxe para a definição de um tipo é
   (struct nome-do-tipo (nome-dos-campos))

Um tipo para representar um ponto contendo coordenadas x e y é:
|#

(struct point (x y))

; Para definir elemento deste tipo (construtor) basta usar o nome do tipo seguidos pelos valores dos campos

(point 1 2) ; define um tipo point com campos x valendo 1 e y 2

; Quando definimos um tipo usando struct, funções para obter os campo e teste são automaticamente criadas
; As funções para obter os campos tem o nome do tipo, um traço seguido pelo nome do campo

(point-x (point 1 2)) ; usa a função point-x para obter o valor x do elemento (point 1 2)
(point-y (point 1 2)) ; usa a função point-y para obter o valor y do elemento (point 1 2)
(point? (point 1 2))  ; usa a função de teste point? para verificar se (point 1 2) é um elemento do tipo point

; Podemos usar a diretiva #:transparent na definição de um struct para dizer oa Racket imprimir o valor semelhante como escrevemos
; Por exemplo, podemos definir o tipo racional da seguinte forma:

(struct rational (p q) #:transparent)

(rational 1 2)

; Podemos implementar o tipo Environment usando struct de Racket, ficando assim:

(struct env-empty () #:transparent)            ; registro para representar um ambiente vazio
(struct env-imp (var value env) #:transparent) ; registro para representar uma extensão de ambiente

(define v2:empty-env
  (env-empty))

(define (v2:extend-env var value env)
  (env-imp var value env))
 

(define (v2:apply-env env var)
  (match env
    [(env-empty) (error "No bind")]
    [(env-imp k v e)
     (if (eq? k var) v
         (v2:apply-env e var))]))
  
(define env2 (v2:extend-env 'd 6    
              (v2:extend-env 'a 1
                  v2:empty-env)))

(v2:apply-env env2 'd)
(v2:apply-env env2 'a)
;(v2:apply-env env2 'c)


#|

Quando a interface do tipo de dados apresenta somente uma operação que manipula o tipo (observer) e as demais operações
são construtores, podemos realizar uma implementação procedural.

A implementação procedural consiste em representar o tipo como funções.
Ou seja, os construtores do tipo criam funções que, quando usadas na operação principal (observer), irão funcionar como esperado.

Por exemplo, a operação principal do Environment é a função apply-env, cujo tipo é:

   - apply-env :: Env -> Var -> Value

Isto é, apply-env recebe como parâmetro um ambiente, uma chave e deve retornar o valor associado a chave no ambiente.
Desde modo, um ambiente deve ser uma função que recebe uma chave como parâmetro e retorna o valor associado no ambiente.
Um ambiente e a função apply-env devem ser:
         - Env = Var -> Value
         - (define (apply-env env var)
                       (env var))

Usando a implementação procedural, a implementação da função apply-env é direto, pois deve ser apenas a aplicação do ambiente nos argumentos.
A complexidade da implementação passar para os construtores, os quais devem retornar função que apresentem o comportamento desejado.

O construtor empty-env deve retornar uma função que quando recebe uma chave emite um erro, afinal este é um ambiente vazio -- não há pares
(chave, valor) associado no ambiente.

O construtor extend-env deve retornar uma função retorna o valor associado a chave.

|#


; Definição Procedural do Environment
(define v3:empty-env
  (lambda (var)                         ; retorna uma função que sempre dar erro para qualquer que seja a chave, var, buscada
    (error "No bind")))

(define (v3:extend-env var value env)
  (lambda (svar)                        ; retorna uma função que dada a chave svar retorna value se for igual a var
    (if (equal? svar var) value
        (v3:apply-env env svar))))      ; faz a busca recursiva se svar e var forem diferentes

(define (v3:apply-env env var)
  (env var))

(define env3 (v3:extend-env 'd 6    
              (v3:extend-env 'a 1
                  v3:empty-env)))

(v3:apply-env env3 'd)
(v3:apply-env env3 'a)
;(v3:apply-env env3 'c)

; Esta versão 3 é a que está implementada no módulo env.rkt!

; Uma vantagem de implementar tipos usando a estratégia procedural é que os usuários do tipo só conseguem fazer uso via interface

#| ****************************************************** EXERCÍCIOS ****************************************************

1) Considere uma representação para os número naturais em uma notação na base $16$.
Nessa base, o número 258 é denotado pela sequência de digitos 102, pois

    258 = 1 × 16² + 0 × 16¹ + 2 × 16⁰

Em Racket, podemos codificar essa representação por meio de listas.
Nesta representação, consideraremos a lista vazia representando o número zero e, por conveniência, o digito menos significativo será a cabeça da lista.
Assim, o número 258 será representado pela lista (2 0 1).

Considerando esta representação, implemente as operações sob número naturais:

a) (zero)
b) (is-zero? n)
c) (suc n)
d) (pred n)

SUGESTÃO: Modifique o módulo nat.rkt com a nova representação.

2) Considere o tipo de dados pilha cuja interface contém as operações
     - empty-stack
     - push
     - pop

a) Implemente as operações de uma pilha em um estilo não procedural
b) Quais dessas operações são para construir elementos do tipo de dados e quais são para obter informações?
c) Implemente as operações em um estilo procedural.

3) Uma árvore binária de inteiros pode ser definida pela seguinte gramática:

     Bintree → () | (Int Bintree Bintree)

Considerando essa representação, implemente:

a) a função number->bintree, que dado um número, produz uma árvore binária contendo esse número como raiz e duas subárvores vazias
b) funções insert-to-left e insert-to-right para inserir um número à esquerda, ou à direita, respectivamente, em uma árvore binária. 

|#
