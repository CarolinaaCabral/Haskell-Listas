import Data.List (nub)

-- Lista 1 
--Quesito 1
{-Fornecidos três valores a, b e c, escreva uma função que retorne quantos dos três são iguais. 
A resposta pode ser 3 (todos iguais), 2 (dois iguais e o terceiro diferente) ou 0 (todos diferentes). -}

funcaoIguais :: Int -> Int -> Int -> Int
funcaoIguais a b c 
    | a == b && b == c = 3
    | a == b || a == c || b == c = 2
    | otherwise = 0 

-- Quesito 2
{-Fornecidos três valores a, b e c, elaborar uma função que retorne quantos 
desses três valores são maiores que a média entre eles.
-}
maiorMedia :: Int -> Int -> Int -> Int
maiorMedia a b c = length [x | x <- [a, b, c], x > ((a + b + c) `div` 3)]

-- Quesito 3
{- Escreva uma função potencia_2 que retorne o quadrado de um número (x2).-}
potencia_2 :: Int -> Int
potencia_2 x = x*x

-- Quesito 4
{- Reutilizando a função potencia_2, construir uma função potencia_4 que retorne o seu argumento elevado à quarta potência. -}
potencia_4 :: Int -> Int
potencia_4 x = potencia_2(potencia_2 x)

-- Quesito 5
{- Implemente em Haskell a função do ou-exclusivo, que é dada por: -}
ouExclusivo :: Bool -> Bool -> Bool
ouExclusivo x y = (x || x) && not (x && y) 

-- Quesito 6
{-Escrever duas funções, x_maior que retorne o maior e x_menor que retorne o menor valor real, 
das raízes de uma equação do segundo grau. A expressão genérica é dada por: Baskara
-}
funcmaior :: Float -> Float -> Float -> Float
funcmaior a b c = (-b + sqrt (b**2 - 4*a*c)/(2*a))

funcmenor :: Float -> Float -> Float -> Float
funcmenor a b c = (-b - sqrt (b**2 - 4*a*c)/(2*a))

-- Quesito 7
{- Criar funções que calculam a soma dos números entre n1 e n2, incluindo e excluindo os limites. -}
ordenarLimit :: Int -> Int -> (Int, Int)
ordenarLimit n1 n2 = (min n1 n2, max n1 n2)

somaIncl :: (Int, Int) -> Int
somaIncl (n1, n2) = sum [n1..n2]

somaExcl :: (Int, Int) -> Int
somaExcl (n1, n2) = sum [(n1+1)..(n2-1)]

-- Quesito 8
{- Dados três números n1, n2 e n3, encontrar os múltiplos de n3 que se encontram no  intervalo [n1,n2] (inclusivo). -}
ordenarDoisPrimeiros :: Int -> Int -> Int -> (Int, Int, Int)
ordenarDoisPrimeiros n1 n2 n3 = (min n1 n2, max n1 n2, n3)

buscarMultiplos :: (Int, Int, Int) -> [Int]
buscarMultiplos (n1, n2, n3) = [x | x <- [n1..n2], x `mod` n3 == 0] 

-- Quesito 9
{- Utilizando a função sum, faça uma função que calcule a multiplicação entre dois números quaisquer, 
considerando números positivos e negativos. Não é permitido usar a operação de multiplicação. -}
somaMult :: Int -> Int -> Int 
somaMult a b 
    | a == 0 || b == 0 = 0
    | (a < 0) /= (b < 0) = negate resulAbs
    | otherwise = resulAbs 
    where
        listaSoma = replicate (abs a) (abs b)
        resulAbs = sum listaSoma

-- Quesito 10
{-  Implemente a função mod2, que retorna o resto de uma divisão de inteiros. OBS:
não é permitido usar a função mod nem a função rem da biblioteca. -}
funcResto :: Int -> Int -> Int
funcResto a b = a - ((div a b)*b) -- div retorna o resultado inteiro da divisão

-- Quesito 11
{- Seja a sequência:
𝑎
1 = √(6)
𝑎
2 = √(6 + √(6))
𝑎
3 = √(6 + √(6 + √(6)))
𝑎
4 =...
Escreva a função que calcula essa sequência. -}
ax :: Int -> Double
ax 1 = sqrt 6
ax x = sqrt(6 + ax(x-1))

-- Quesito 12
{- Implementar a fórmula que indica de quantas maneiras é possível escolher n objetos
de uma coleção original de m objetos, onde m >= n. -}
combinatoria :: Int -> Int -> Int
combinatoria n m = (product [1..m]) `div` (product [1..n] * product[1..(m - n)])

-- Quesito 13
{-  Defina uma função que, dada uma lista numérica, retorne uma tupla, que contenha o
maior da lista bem como sua posição relativa. -}


maiorPosic :: Ord a => [a] -> (a, Int)
maiorPosic lista = (maximum lista, [indice | (indice, numero) <- zip[0..] lista, numero == maximum lista] !! 0)

-- Quesito 14
{-  Defina uma função que converta uma lista de dígitos (unitários, 0 a 9) em uma outra
lista, que é a sua tradução em String. Considere um dicionário do tipo:
dic_10 = [(0,"zero"), (1,"um"), (2,"dois"), …, (9,"nove")] -}
dic_10 :: [(Int, String)]
dic_10 = zip [0..9] ["zero", "um", "dois", "três", "quatro", "cinco", "seis", "sete", "oito", "nove"]

traducao :: [Int] -> [String]
traducao numero = [valor | n <- numero, (chave, valor) <- dic_10, chave == n]

-- Quesito 15
{- Construa uma função del_posicao_n :: [Int] -> Int -> [Int] em que
dada uma lista de inteiros e a posição de um elemento qualquer, retorne uma nova
lista sem aquele elemento da n-ésima posição. -}
del_posicao_n :: [Int] -> Int -> [Int]
del_posicao_n lista n = [x | x <- lista, x /= lista !! n]

-- Quesito 16
{- Construa uma função inserir_posicao_x :: [Int] -> Int -> Int -> [Int] em que, dada uma
lista de inteiros, uma posição e um elemento a ser inserido, retorne uma nova lista
com aquele elemento na n-ésima posição. -}
inserir_posicao_x :: [Int] -> Int -> Int -> [Int]
inserir_posicao_x lista posicao elemento = take posicao lista ++ [elemento] ++ drop posicao lista

-- Quesito 17
{- Defina uma função que retorne o valor da n-ésima posição de uma lista. -}
valorPosicao :: [a] -> Int -> a
valorPosicao lista n = lista !! n

-- Quesito 18 
{- Dadas duas listas ordenadas como entrada, faça uma função merge, que une as
duas listas. -}
uniaoListas :: Ord a => [a] -> [a] -> [a]
uniaoListas [] lista2 = lista2
uniaoListas lista1 [] = lista1

uniaoListas (x:xs) (y:ys)
    | x <= y      = x: uniaoListas xs (y:ys)
    | otherwise   = y: uniaoListas (x:xs) ys

-- Quesito 19
{- Implemente uma função que receba duas listas de inteiros sem repetição, e retorne
uma terceira lista que contenha somente números que estejam nas duas listas
dadas.. -}
interseccao :: [Int] -> [Int] -> [Int]
interseccao lista1 lista2 = nub [x | x <- lista1, x `elem` lista2] 

-- Quesito 20
{- Crie uma função que faça uma codificação sobre uma sequência de caracteres
iguais, substitua a sequência por !na, onde n é o número de vezes que o caractere a 
é repetido. 
Observe que só devem ser comprimidas sequências de tamanhos
maiores que 3. 
Exemplo:
comprime "asdffffghjjkllllpoooi"
"asd!4fghjjk!4lpoooi" -}


-- Quesito 21
-- casa padrão com lista

                