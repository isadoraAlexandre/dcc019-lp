********************************* Tipos de Dados Algébricos ************************************

Tipos de dados em Haskell são definidos usando a palavra "data":

> data Boolean = V | F
> data Color = Red | Blue | Green | Pink

Note que os construtores são separados pelo símbolo |
Valores do tipo são criado usando um dos construtores.
São exemplos de valores dos tipos Boolean e Color:

    - V
    - F
    - Red
    - Blue
    - Green
    - Pink

- Construtores de tipos podem ter parâmetros.
Por exemplo, suponhamos a definição de um tipo de dados
para representar Formas (Shape):
   * Círculo com raio e coordenadas (x,y) do centro
   * Retângulo com as coordenadas (x,y) do ponto inferior mais à esquerda
e ponto superior mais à direita

> data Shape = Circle Float Float Float
>            | Rectangle Float Float Float Float deriving Show

Valores são construído usando os construtores e os argumentos.
São exemplos de valores do tipo Shape:

   - Circle 1.0 2.0 2.0
   - Rectangle 1.0 1.0 3.0 3.0

- OBSERVAÇÃO: Os construtores são funções dos tipos dos parâmetros para o tipo de dados
Usando o ghci, obtenha o tipo dos construtores Circle e Rectangle!

   > :t Circle
   > :t Rectangle


- O casamento de padrão pode ser usado para definir funções com tipos de dados:

  * Função de implicação para o tipo Boolean

> impl _ V = V
> impl V F = F

  * Área de um tipo Shape

> area (Circle x y r) = pi * r * r
> area (Rectangle x1 y1 x2 y2) = abs (y2 - y1) * abs (x2 - x1)



> v = Circle 0 0 1

- Construtores de tipos são funções, portanto podemos aplicá-los parcialmente!

*********************************** EXERCÍCIOS ***********************************************

1- Qual o resultado da expressão?
  map (Circle 10 20) [4, 5, 6, 6]

**********************************************************************************************

- Considere agora que desejamos criar um tipo de dados para representar uma pessoa contendo as
informações do primeiro e último nome, idade, peso, telefone e time que torce.

Podemos criar o seguinte tipo para representar uma pessoa:

> data Person = Person String String Int Float String String deriving Show

Um exemplo de valor deste tipo seria:

    - Person "João" "Silva" 42 80.7 "(32) 98877-0125" "Flamengo"

Podemos fazer funções para obter o nome, idade, etc. :

> fisrtname (Person n _ _ _ _ _) = n
> lastname (Person _ l _ _ _ _) = l
> idade (Person _ _ age _ _ _) = age

Quanto construtores do tipo de dados tem vários argumentos, a definição de
funções fica muito verboso e propenso a erro (temos que memorizar a ordem
exata dos argumentos).

Haskell tem uma sintaxe alternativa para definir tipos de dados.
Usando uma sintaxe de registros, nomeamos cada um dos campos:

> data Pessoa = Pessoa {
>                  fname :: String,
>                  lname :: String,
>                  age :: Int,
>                  weight :: Float,
>                  phone :: String,
>                  team :: String
>                } deriving Show

Usando a sintaxe de registro, automaticamente são criadas funções de projeção
para cada um dos campos do tipo de dados

*********************************** EXERCÍCIOS ***********************************************

2- Crie um tipo de dados usando a sintaxe de registro para representar um carro contendo
as informações da marca (company), modelo (model) e ano de fabricação (year)

Use as funções de projeção criadas para obter cada um dos campos de um valor (definido por você).

Qual o tipo das funções de projeção?

**********************************************************************************************

Suponha que desejamos criar uma pilha de inteiros. Uma alternativa seria criar um tipo de dados
para representar um pilha de inteiros:

> data StackInt = StackInt [Int]

Usando esta definição, podemos criar funções que manipulam a pilha:

> pushInt :: StackInt -> Int -> StackInt
> pushInt (StackInt stk) n = StackInt (n : stk)
> popInt :: StackInt -> (Int, StackInt)
> popInt (StackInt (x:xs)) = (x, StackInt xs)

E se quisermos uma pilha de String ou qualquer outro tipos?


- Tipos Algébricos podem ser parametrizados por tipos!
Assim, podemos definir uma pilha parametrizada pelo tipo do valor:

> data Stack a = Stack [a]

> push :: Stack a -> a -> Stack a
> push (Stack xs) x = Stack (x:xs)
> pop :: Stack a -> (a, Stack a)
> pop (Stack (x:xs)) = (x, Stack xs)

Um tipo parametrizado comum na linguagem Haskell é o Maybe, usado para
definir situações que podem ocorrer erro, que é assim definido:

data Maybe a = Nothing | Just a

Para facilitar a legibilidade, podemos definir tipos sinônimos usando a
palavra reservada type facil

> type PilhaInt = Stack Int

Haskell permite a definição de tipos recursivos, Por exemplo, um tipo de
árvore binária pode ser assim definido:

> data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

Usando casamento de padrão, podemos definir uma função de busca em árvores:

> search :: (Num a, Ord a) => a -> Tree a -> Bool
> search e Empty = False
> search e (Node x left right)
>     | e == x = True
>     | e < x = search e left
>     | otherwise = search e right

*********************************** EXERCÍCIOS ***********************************************

3- Faça uma função para inserir (insert :: a -> Tree a -> Tree a) um elemento em uma árvore binária

4- Qual o resultado da avaliação da expressão:

    foldr insert Empty [5, 1, 4, 2, 3]

5- Seja a definição da linguagem da lógica proposicional, formada pelo conjunto de todas as
fórmulas (F) definidas recursivamente da seguinte forma:
  - variáveis proposicionais, denotadas pelas letras minúsculas p, q, r e s, possivelmente com
índices (0, 1, 2, ...), são fórmulas;
  - os símbolos ⊥ e ⊤ são fórmulas, que representam os valores lógicos falso e verdadeiro, respectivamente;
  - se α é uma fórmula, então ¬α é uma fórmula;
  - se α e β são fórmulas, então (α ∨ β), (α ∧ β), (α → β) e (α ↔ β) são fórmulas.

Uma variável proposicional pode assumir apenas dois valores lógicos, verdadeiro ou falso, assim podemos
construir uma tabela com todas as possíveis combinações de valores que uma variável proposicional pode
assumir e determinar o valor lógico de fórmulas mais complexas, a partir do significado dos seus conectivos lógicos.
Essa tabela é denominada de tabela verdade.
Quando uma fórmula tem valor lógico sempre verdade, independente dos valores lógicos das proposições que a
compõem, dizemos que a fórmula é uma tautologia.
Os itens subsequentes têm como objetivo construir um programa que verifica se uma fórmula lógica é uma tautologia.

a) crie um tipo de dados para representa a linguagem da lógica proposicional.
Considere que uma variável proposicional será representada somente por um caractere.

b) desenvolva um tipo de dados que associa valores lógicos à variáveis proposicionais.
Crie funções que dada uma associação e uma variável retorna o valor lógico da mesma (look) e uma que adiciona
um valor lógico de uma variável proposicional à uma associação (add).
Por exemplo, em uma associação que contém a variável p associado ao valor lógico True e q ao valor lógico False,
a operação look de p deve retornar True e look de q deve retorna False.

Já a expressão add r False tem como efeito criar um nova associação, a partir da anterior, na qual a variável
proposicional r tem valor lógico False.


c) defina a função eval que tem como parâmetros uma associação de variáveis proposicionais para valores lógicos
e uma fórmula e, tem como resultado, o valor lógico da fórmula.


d) defina uma função que dada uma fórmula retorna uma lista que contém todas as variáveis proposicionais da fórmula.

e) usando a função bools, conforme definida a seguir, que retorna todas as combinações possíveis de valores lógicos
com n posições (n inteiro maior que 1), faça uma função que determina se uma fórmula é uma tautologia.

> bools :: Int-> [[Bool]]
> bools 1 = [[True], [False]]
> bools n = (map (True:) (bools (n-1))) ++ (map (False:) (bools (n-1)))


6- Defina um tipo de dado que pode representar uma árvore n-ária.
Em seguida:

a) defina uma função que retorne a altura da árvore
b) defina uma função que verifica quantos filhos tem a raiz

**********************************************************************************************
