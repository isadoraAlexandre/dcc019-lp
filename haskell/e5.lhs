************************************ Mônada de Estado ************************************

Considere que você está fazendo um programa e precisa representar uma pilha e realizar uma
sequência de operações. Por exemplo:

        push 3 => pop => push 5

Certamente, definimos um tipo de dados para representar uma pilha e as funções associadas

> import Control.Monad.State
> import Data.Functor.Identity

> data Stack a = Stack [a] deriving (Show, Eq)

> pop :: Stack a -> (a, Stack a)
> pop (Stack (x:xs)) = (x, Stack xs)

> push :: a -> Stack a -> ((), Stack a)
> push x (Stack xs) = ((), Stack (x:xs))

Observe que, como não há estado em programa funcional, programas que precisam do conceito
de estado, como um programa que manipula um pilha, devem ter o estado como retorno para
poder encadear a sequência de operações.

Podemos dizer que uma operação de estado deve ter o estado atual como parâmetro e tem como
resultado um par com o resultado da operação e novo estado:

                  s -> (a, s)

Assim, as funções pop e push foram implementadas como operções que manipulam estado, retornando
uma tupla com o resultado da operação e o novo estado da pilha.
Como push apenas retorna um novo estado sem produzir valor, foi implementada como uma par vazio
e a nova pilha (poderia retorna apenas uma pilha, mas para efeito de padronização,
foi implementado uma operação sob estado). A função pop, retorna o par representando o elemento
removido e a nova pilha

Portanto, uma função que realiza a sequência de operações
      push 3
      pop
      push 5

Pode ser implementada como:


> foo :: Stack Int -> ((), Stack Int)
> foo s = let
>   ((), s1) = push 3 s   -- realiza a operação push no estado s e retorna um novo par
>   (a, s2) = pop s1      -- realiza a operação pop no novo estado, retornando um outro par
>   in push 5 s2          -- faz o push 5 no último estado, retornando um par (valor, estado)


Um incoveniente para trabalhar com o estado pilha é que a todo momento é preciso capturar o
par resultante para operação de estado e, explicitamente, passar o novo estado para a operação seguinte.
Idealmente, gostaríamos de realizar este sequênciamente de operações no estado da sequinte forma:

   do
     push 3
     a <- pop
     push 5


Para isto, vamos criar um tipo para representar a noção de operações com estados da pilha:

> data SState s a = SState {runSState :: Stack s -> (a, Stack s)}

A representação do tipo para rerpesentar um estado da pilha é parametrizado pelo estado, tipo da pilha (s),
e o tipo do resultado da operação (a). Este tipo tem uma função que encarregado de executar uma operação
sobre o estado (runSState).

Para usar o sequenciamento de operações (notação do), precisamos fazer este tipo uma instância de Monad
(por consequência, de Functor e Applicative também)

fmap :: (a -> b) -> f a -> f b
data Ex a = Con {fun:: (a -> a)}
ghci> e1 = Con (\x -> x)

[\x -> x + 1] <*> [2,3]
Just (\x -> x + 2) <*> Just 3

> instance Functor (SState s) where
> -- fmap :: (a -> b) -> SState s a -> SState s b
>    fmap f (SState h) = SState $ \s -> let (v, ns) = h s in (f v, ns)

> instance Applicative (SState s) where
>  -- pure :: a -> SState s a
>  pure x = SState $ \s -> (x,s)
>  -- <*> :: SState s (a -> b) -> SState s a -> SState s b
>  (SState h) <*> (SState g) = SState $ \s -> let (f, s1) = h s
>                                                 (v, s2) = g s1
>				              in (f v, s2)


> instance Monad (SState s) where
>    -- return :: a -> SState s a
>    return x = SState $ \s -> (x,s)
>    -- >>= :: SState s a -> (a -> SState s b) -> SState s b
>    (SState h) >>= f = SState $ \s -> let (value, newState) = h s
>                                          state = f value
>                                      in (runSState state) newState


Usando o estado SState, as funções pop e push ficam:

> empty :: Stack a
> empty = Stack []

> initStack :: [a] -> Stack a
> initStack xs = Stack xs

> push1 :: a -> SState a ()
> push1 x = SState $ \(Stack xs) -> ((), Stack (x:xs))

> pop1 :: SState a a
> pop1 = SState $ \(Stack (x:xs)) -> (x, Stack xs)

E a sequência de operações agora pode ser implementada como a notação do:

> boo :: SState Integer ()
> boo = do
>    push1 3
>    a <- pop1
>    push1 5

Assim, podemos executar a sequência de operação a partir de um estado, como

     runSState boo empty
ou
     runSState boo $ initStack [1..10]


Haskell tem a mônada State s a, cujo funcionamento é semelhante ao estado ao SState.
Assim, caso precise manipular estados, basta usar o tipo State.
Usando State, poderíamos implementar as funções pop e push:

> push2 :: a -> State (Stack a) ()
> push2 x = StateT $ \(Stack xs) -> Identity ((), Stack (x:xs))

> pop2 :: State (Stack a) a
> pop2 = StateT $ \(Stack (x:xs)) -> Identity (x, Stack xs)

> boo2 = do
>   push2 3
>   pop2
>   push2 5

     runState boo2 empty
ou
     runState boo2 $ initStack [1..10]

************************************ Exercícios ************************************

1) Crie um tipo de dados para representar árvores binários e implemente uma operações
de inserção e remoção como uma mônada de estado, tal como foi feito com pilha.
