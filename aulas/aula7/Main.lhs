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

-

> data Exp = Const Int
>          | Add Exp Exp
>          | Mult Exp Exp
>          deriving (Eq, Ord, Show)


> data Instr = Times
>            | Plus
>            | Number Int
>            deriving (Eq, Ord, Show)


> type Program = [Instr]

> compile' :: Exp -> Program
> compile' (Const n) = [Number n]
> compile' (Add e e') = compile' e' ++ compile' e ++ [Plus]
> compile' (Mult e e') = compile' e' ++ compile' e ++ [Times]


> data ExpAlg a = ExpAlg {
>                   constA : 
>                 }
