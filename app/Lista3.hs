-- Esta lista aborda o conceito de funções recursivas

-- Quesito 1
{- Como a versão recursiva da função fatorial se comporta se dermos a ela como argumento um número negativo? 
Modifique a implementação clássica para não permitir números negativos adicionando uma guarda ao passo recursivo.
 -}

fatorial :: Int -> Int
fatorial n | n < 0     = error "Fatorial não definido para números negativos"
            | n == 0    = 1
            | otherwise = n * fatorial (n - 1)

-- Quesito 2
{- Defina a função recursiva somar :: Int -> Int que retorna a soma dos inteiros 
não-negativos a partir de um valor até zero. Por exemplo, somar 3 deve retornar 3+2+1+0 = 6. -}
soma :: Int -> Int
soma n | n < 0     = error "Soma não definida para números negativos"
    | n == 0    = 0
    | otherwise = n + soma (n - 1)

-- Quesito 3
{- Defina o operador de exponenciação ^ utilizando uma função recursiva, 
semelhante ao padrão usado para implementar a multiplicação com o operador *:
(*) :: Num a => a -> a -> a
m * 0 = 0
m * n = m + (m * (n - 1))
 -}
exponencial :: (Eq a, Num a) => a -> a -> a
exponencial m n 
            |n == 0 = 1
            |otherwise = m * exponencial m (n - 1)

-- Quesito 4
{- Defina a função euclides :: Int -> Int -> Int que implementa o algoritmo de Euclides 
para calcular o máximo divisor comum de dois inteiros não-negativos: se dois números são iguais, 
este número é o resultado; caso contrário, o menor número é subtraído do maior e o processo é repetido 
passando este novo número e o menor valor passado anteriormente como argumento. Exemplo:
> euclides 6 27
3
 -}
euclides :: Int -> Int -> Int
euclides x y 
    | x < 0 || y < 0 = error "Função de Euclides não definida para números negativos"
    | x == y         = x
    | x > y          = euclides (x - y) y
    | otherwise      = euclides x (y - x)

-- Quesito 5
{- Defina as funções abaixo usando recursão: -}
{- a) Decidir se todos os valores em uma lista são True:
andList :: [Bool] -> Bool -}
andList :: [Bool] -> Bool
andList [] = True
andList (x:xs) = x && andList xs

{- b) Concatenar uma lista de listas:
concatList :: [[a]] -> [a] -}
concatList :: [[a]] -> [a]
concatList [] = []
concatList (x:xs) = x ++ concatList xs

{- c) Produzir uma lista com n elementos idênticos:
replicate :: Int -> a -> [a] -}
replicateList :: Int -> a -> [a]
replicateList n x
        |n <= 0    = []
        |otherwise = x : replicateList (n - 1) x

{- d) Selecionar o n-ésimo elemento em uma lista:
(!!) :: [a] -> Int -> a -}
indice :: [a] -> Int -> a
indice [] _ = error "Índice fora de lista"
indice (x:xs) 0 =  x
indice (x:xs) n = indice xs (n - 1)
           

{- e) Decidir se um valor está presente em uma lista:
elem :: Eq a => a -> [a] -> Bool -}
elemList :: Eq a => a -> [a] -> Bool
elemList _ [] = False 
elemList n (x:xs)
            |x == n    = True
            |otherwise = elemList n xs

-- Quesito 6 
{- Definir a função recursiva merge :: Ord a => [a] -> [a] -> [a] 
que une duas listas ordenadas em uma lista ordenada. Exemplo:
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6] -}
mergeList :: Ord a => [a] -> [a] -> [a]
mergeList [] ys = ys
mergeList xy [] = xy
mergeList (x:xs) (y:ys)
            |x <= y    = x : mergeList xs (y:ys)
            |otherwise = y : mergeList (x:xs) ys

-- Quesito 7
{- Usando a função merge, defina a função mergesort :: Ord a => [a] -> [a] que implementa 
o algoritmo de ordenação merge sort, que por sua vez considera uma lista vazia e uma lista 
com apenas um elemento como listas ordenadas, e que qualquer outra lista é ordenada a partir 
da união de duas listas que resultaram da ordenação das suas metades separadamente. 
Dica: primeiro implemente a função metades :: [a] -> ([a],[a]) que separa uma lista em duas 
partes cujos comprimentos são iguais ou no máximo diferem em uma unidade. -}
metades :: [a] -> ([a], [a])
metades xs = (take n xs, drop n xs)
  where n = length xs `div` 2

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []                             
mergeSort [x] = [x]                            
mergeSort xs  = mergeList (mergeSort esq) (mergeSort dir) 
  where (esq, dir) = metades xs

-- Quesito 8 
{- Implemente recursivamente funções que: -}
{- a) calcule a soma de uma lista de inteiros; -}
somaList :: [Int] -> Int
somaList [] = 0
somaList (x:xs) = x + somaList xs

{- b) obtenha o número de elementos de uma lista; -}
lengthList :: [a] -> Int
lengthList [] = 0
lengthList (x:xs) = 1 + lengthList xs

{- c) selecione o último elemento de uma lista não-vazia. -}
lastList :: [a] -> a
lastList [x] = x
lastList (x:xs) = lastList xs