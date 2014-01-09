% Compilando Expressões
% Prof. Rodrigo Ribeiro
% Programação Funcional em Haskell

Compilando Expressões --- (I)
=============================

- Objetivo: Construir um compilador de uma linguagem de expressões.
     - Usar Haskell para implementação.
     - Modelar as estruturas de dados necessárias como tipos de dados algébricos.
     - Usar funções de ordem superior para o processo de compilação.

Compilando Expressões --- (II)
==============================

- Quer dizer que vamos gerar código executável?
     - Sim! Usaremos uma técnica comum em compiladores: uma linguagem intermediária.
- Porque usar uma linguagem intermediária?
     - Muitas vezes, para linguagens reais, a tradução para código de máquina pode ser difícil.
     - Nesse sentido, é mais fácil traduzir gradualmente a linguagem fonte para a linguagem de máquina.

Compilando Expressões --- (III)
===============================

- Linguagem fonte, expressões envolvendo:
    - Constantes inteiras, adição e multiplicação.
- Nesse primeiro momento, vamos usar essa linguagem simples e traduzí-la por completo.


Compilando Expressões --- (IV)
===============================

- Linguagem intermediária a ser usada:
     - Uma máquina de pilha
- Mais sobre máquinas de pilha:
     - Não possuem registradores ou acessam memória.
     - Única memória disponível: pilha
- Pilha
     - Estrutura de dados com política LIFO --- Last In, First Out

Compilando Expressões --- (V)
===============================

- Expressões --- nossa linguagem fonte:

> data Exp = Const Int
>          | Add Exp Exp
>          | Mult Exp Exp
>          deriving (Eq, Ord, Show)

Compilando Expressões --- (VI)
===============================

- Instruções --- nossa linguagem intermediária:

> data Instr = Times
>            | Plus
>            | Number Int
>            deriving (Eq, Ord, Show)

Compilando Expressões --- (VII)
===============================

- A expressão $3 + 2 * 5$ é representada como:

> e :: Exp
> e = (Const 2) `Add` ((Const 3) `Mult` (Const 5))

Compilando Expressões --- (VIII)
================================

- O resultado de compilação da expressão anterior deve ser:

> c :: Program
> c = [Number 3, Number 5, Times, Number 2, Plus]

Compilando Expressões --- (IX)
==============================

- Semântica de expressões

> exec :: Exp -> Int
> exec (Const n) = n
> exec (Add e e') = exec e + exec e'
> exec (Mult e e') = exec e * exec e'

Compilando Expressões --- (X)
==============================

- Semântica da máquina de pilha:

> type Stack = [Int]

> eval :: Program -> Stack -> Stack
> eval [] s = s
> eval (i:is) s = eval is (step i s)
>       where
>         step (Number n) s' = n : s'
>         step Times (x:x':s') = x * x' : s'
>         step Plus  (x:x':s') = x + x' : s'

Compilando Expressões --- (XI)
===============================

- Compilando expressões:

> type Program = [Instr]

> compile :: Exp -> Program
> compile (Const n) = [Number n]
> compile (Add e e') = compile e' ++ compile e ++ [Plus]
> compile (Mult e e') = compile e' ++ compile e ++ [Times]

Compilando Expressões --- (XII)
==============================

- Testando o processo de compilação:
     - Este é correto se compilar uma expressão e executar as instruções
       produz o mesmo resultado que executar a expressão, isso é:

> correct :: Exp -> Bool
> correct e = head (eval (compile e) []) == exec e


Compilando Expressões --- (XIII)
================================

- Gerando código para o assemblador NASM...


