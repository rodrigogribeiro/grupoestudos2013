% Definição de Tipos em Haskell
% Prof. Rodrigo Ribeiro
% Programação Funcional em Haskell

Definição de Tipos em Haskell --- (I)
=================================

- Definindo tipos de dados em Haskell
     - Sinônimos de tipos.
     - Renomeamento de tipos.
     - Tipos de dados algébricos.
            - Sintaxe para registros.

Definição de Tipos em Haskell --- (II)
======================================

- Sinônimos de tipos
   - Usado para dar um novo "nome" para um tipo.
   - O tipo original e o "novo" podem ser intercambiados.

> type Name = String

> doubleName :: Name -> Name
> doubleName n = n ++ n

> f :: String -> Name
> f = doubleName

Definição de Tipos em Haskell --- (III)
=======================================

- Renomeamento de tipos
    - Permite dar um novo "nome" a um tipo existente.
    - Novo tipo não é intercambiável com novo tipo.

> newtype Age = Age Int

> g :: Age -> Bool
> g (Age x) = x > 0

Definição de Tipos em Haskell --- (IV)
======================================

- Note que Age e Int são tipos diferentes...

~~~~~~~~~~~~~~~{.haskell}
h = g 0 -- wrong

h = g (Age 0) -- ok
~~~~~~~~~~~~~~~~~~~~~~~~~

Definição de Tipos em Haskell --- (V)
=====================================

- Tipos de dados algébricos
     - Mecanismo mais expressivo para definição de tipos
     - Tipos podem ser definidos como um conjunto de construtores de dados
     - Cada construtor de dados, especifica um formato de um valor do tipo.

Definição de Tipos em Haskell --- (VI)
======================================

- Exemplo:
    - Construtor INil: árvore "vazia"
    - Construtor INode: árvore contendo um inteiro e duas subárvores.

> data IntTree = INil | INode Int IntTree IntTree

Definição de Tipos em Haskell --- (VII)
======================================

- Definindo funções sobre tipos de dados algébricos

> sumIntTree :: IntTree -> Int
> sumIntTree INil = 0
> sumIntTree (INode n l r) = n + sumIntTree l + sumIntTree r

Definição de Tipos em Haskell --- (VIII)
========================================

- Mais exemplos...
     - Um tipo de dados algébrico para representar clientes:

~~~~~~~~~~~~~~~~~{.haskell}
data Client = Client String  -- nome
                     String  -- endereço
                     Int     -- idade
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Definição de Tipos em Haskell --- (IX)
========================================

- Sobre a definição anterior...
     - Pouco informativa. Como diferenciar a primeira da segunda String?

~~~~~~~~~~~~~~~~~{.haskell}
type Name = String
type Address = String
type Age = Int

data Client = Client Name
                     Address
                     Age
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Definição de Tipos em Haskell --- (X)
=====================================

- Funções sobre o tipo Client:

~~~~~~~~~~~~~~~~~{.haskell}
type Name = String
type Address = String
type Age = Int

data Client = Client Name Address Age

name :: Client -> Name
name (Client n _ _) = n

address :: Client -> Address
address (Client _ a _) = a

age :: Client -> Age
age (Client _ _ a) = a
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Definição de Tipos em Haskell --- (XI)
======================================

- Definições repetitivas...
    - Somente para obter parte da informação de um tipo de dados.
- Solução: Definição usando sintaxe de registro.
    - Vantagens: funções de projeção definidas automaticamente.

> type Address = String
> data Client = Client {
>                 name :: Name,
>                 address :: Address,
>                 age :: Age
>               }

Definição de Tipos em Haskell --- (XII)
=======================================

- Tipos de dados podem ser polimórficos!
- Exemplos:

~~~~~~~~~~~{.haskell}
 type IntRight a = (a,Int)

 newtype DiffList a = DiffList { out :: [a] -> [a] } 
~~~~~~~~~~~

> data Tree a = Leaf
>              | Node a (Tree a) (Tree a)
>              deriving (Eq, Ord, Show)

Definição de Tipo em Haskell --- (XIV)
======================================

- Tipos polimórficos:
    - Variáveis de tipos representam "qualquer" tipo.
- Exemplos de tipos polimóricos: listas, tuplas
- Mais exemplos

~~~~~~~~~~~~~~~~{.haskell}
Tree Int, Tree (a,Bool),
[Tree Char],
([Char], Tree String)...
~~~~~~~~~~~~~~~~~~~~~~~~~~


Definição de Tipos em Haskell --- (XIII)
========================================

- Exemplos: Funções sobre árvores polimórficas

> insert :: Ord a => a -> Tree a -> Tree a
> insert x Leaf = Node x Leaf Leaf
> insert x (Node y l r)
>       | x < y = Node y (insert x l) r
>       | otherwise = Node y l (insert x r)


Definição de Tipos em Haskell --- (XIV)
=======================================

- Mais funções...

> member :: Ord a => a -> Tree a -> Bool
> x `member` Leaf = False
> x `member` (Node y l r) = x == y       ||
>                           x `member` l ||
>                           x `member` r

> size :: Tree a -> Int
> size Leaf = 0
> size (Node x l r) = 1 + size l + size r

Definição de Tipos em Haskell --- (XV)
======================================

- Note que as funções member e size, possuem um padrão:
    - Retornar um valor quando a árvore é vazia
    - Aplicar uma função aos valores do nó e aos resultados de processar recursivamente as subárvores.
- Esse padrão é similar ao foldr para listas!

> fold :: (a -> b -> b -> b) -> b -> Tree a -> b
> fold f v Leaf = v
> fold f v (Node x l r) = f x (fold f v l) (fold f v r)

Definição de Tipos em Haskell --- (XVI)
======================================

- Com isso, size e member ficam:

> size' :: Tree a -> Int
> size' = fold (\x y z -> 1 + y + z) 0

> member' :: Ord a => a -> Tree a -> Bool
> member' x = fold (\k y z -> x == k || y || z) False

Definição de Tipos em Haskell --- (XVII)
========================================

- Mas exemplos, usando fold:
    - Caminhamento em pré-ordem

> preorder :: Tree a -> [a]
> preorder = fold (\x l r -> x : (l ++ r)) []

Definição de Tipos em Haskell --- (XVIII)
========================================

- Próxima semana...
     - Um exemplo de tipos de dados algébricos: Um compilador de expressões aritméticas para uma máquina de pilha!
     - Reunião na quarta-feira às 18:00 hrs!

Definição de Tipos em Haskell --- (XIX)
========================================

- Tarefas para o recesso:
     - Exercícios para serem resolvidos e apresentados na primeira sexta-feira.
     - Todo o código produzido por vocês deve ser disponibilizado no github.
- Como organizar seu código no github:
     - Crie um repositório chamado cursoHaskell2013
     - Coloque suas soluções para cada lista de exercícios em uma pasta
       chamada listaX, onde X é o número da lista de exercícios em questão.
