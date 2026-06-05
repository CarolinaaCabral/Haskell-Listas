
-- Quesito 1
{- Usando compreensão de listas, forneça uma expressão que calcula a soma
1² + 2² +... + 100² dos quadrados dos primeiros 100 números inteiros. -}
somaQuadrados :: Int
somaQuadrados = sum [x^2 | x <- [1..100]]

-- Quesito 2
{- Suponha que um plano de coordenadas de tamanho m x n é dado pela lista de todos
os pares (x,y) de inteiros tal que 0 ≤ 𝑥 ≤ 𝑚 e 0 ≤ 𝑦 ≤ 𝑛. Usando compreensão
de listas, defina a função grid :: Int -> Int -> [(Int,Int)] que retorna
o plano de coordenadas de um dado tamanho. Por exemplo:
> grid 1 2
[(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)] -}

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]] 

-- Quesito 3
{- Usando compreensão de listas e a função grid definida na questão anterior, 
defina uma função quadrado :: Int -> [(Int,Int)] que retorna um plano de coordenadas quadrado de tamanho n, 
excluindo a diagonal principal (0,0) a (n,n). Por exemplo:
> quadrado 2
[(0,1), (0,2), (1,0), (1,2), (2,0), (2,1)] -}
quadrado :: Int -> [(Int,Int)]
quadrado n = [(x,y) | (x,y) <- grid n n, x /= y]

-- Quesito 4
{- De maneira similar à função length, mostre como a função replicate :: Int -> a -> [a] que produz uma lista de 
elementos idênticos pode ser definida usando compreensão de listas. Exemplo:
> replicate 3 True
[True, True, True] -}
funcReplicate :: Int -> a -> [a]
funcReplicate n elemento = [elemento | _ <- [1..n]]

-- Quesito 5
{- Uma tupla (x,y,z) de inteiros positivos é Pitagoreana se satisfaz a equação x² + y² = z². 
Usando compreensão de listas com três geradores, defina a função pitag :: Int -> [(Int, Int, Int)] que retorna 
uma lista de todas as tuplas que satisfazem a condição estabelecida e cujos componentes são menores ou iguais 
a um dado limite. Exemplo: 
> pitag 10
[(3,4,5), (4,3,5), (6,8,10), (8,6,10)]
Uma tupla (x,y,z) de inteiros positivos é Pitagoreana se satisfaz a equação x² + y² = z². 
Usando compreensão de listas com três geradores, defina a função pitag :: Int -> [(Int, Int, Int)] que 
retorna uma lista de todas as tuplas que satisfazem a condição estabelecida e cujos componentes são menores
 ou iguais a um dado limite. Exemplo: 
> pitag 10
[(3,4,5), (4,3,5), (6,8,10), (8,6,10)] -}
pitag :: Int -> [(Int, Int, Int)]
pitag n = [(x, y, z) | x <- [1..n], 
                       y <- [1..n], 
                       z <- [0..n],
                        x^2 + y^2 == z^2]

-- Quesito 6
{- Um inteiro positivo é perfeito se ele é igual à soma de todos os seus fatores, excluindo o próprio número. 
Usando compreensão de listas e a função fatores, defina a função perfeitos :: Int -> [Int] 
que retorna a lista de todos os números perfeitos menores que um limite informado como argumento. i
Exemplo:
> perfeitos 500
[6, 28, 496] -}
perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1..n], sum [d | d <- [1..x-1], x `mod` d == 0] == x]

-- Quesito 7
{- Mostre que a compreensão de lista [(x,y) | x <- [1,	2], y <- [3,4]], com dois geradores, 
pode ser representada usando duas compreensões de lista, cada uma com apenas um gerador. 
Dica: Procure usar a função concat.  -}
duasListas :: [(Int, Int)]
duasListas = concat [[(x,y) | y <- [3, 4]] | x <- [1, 2]]

-- Quesito 8
{- Redefina a função posicoes usando a função buscar, disponível em
https://emanoelbarreiros.github.io/funcional/haskell-5#a-fun%C3%A7%C3%A3o-zip
--
Dica: Procure usar a função zip. -}
buscar :: Eq a => a -> [(a, b)] -> [b]
buscar k xs = [ v |(k', v) <- xs, k' == k]

posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = buscar x (zip xs [0..])

-- Quesito 9
{- Escreva a função capaz de calcular o produto escalar de duas listas de inteiros xs e ys de tamanho n, 
que é dado pelo produto dos inteiros em posições correspondentes:
     n-1
      Σ  (xs_i * ys_i)
     i=0
Dica: Procure usar a função zip. -}
prodEscalar :: [Int] -> [Int] -> Int
prodEscalar xs ys = sum [x*y | (x, y) <- zip xs ys]



