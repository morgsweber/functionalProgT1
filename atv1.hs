-- 98703-02 - Programação Funcional - Turma 010 - 2021/2 - Prof. Diego Vrague Noble
-- Nomes: Arthur Luz, Daniela Rigoli, Morgana Weber

-- 1) Defina uma função recursiva insere :: Int -> [Int] -> [Int] 
-- que insere um inteiro na posição correta em uma lista de inteiros já ordenada. 
-- Mostre o passo-a-passo da aplicação de insere 3 [1,2,4,5]
insere :: Int -> [Int] -> [Int]
insere n []                  = n : []
insere n (x:xs) | n <= x     = n : x : xs
                | otherwise  = x : (insere n xs)

-- insere 3 [1,2,4,5]
-- []
-- (3 <= 1) [1] 
-- (3 <= 2) [1,2]
-- (3 <= 3) [1,2,3] ++ [4,5,6]
-- (3 <= 4) [1,2,3,4,5,6]


-- 2) Usando a função insere, defina a função ordenaInsere :: [Int] -> [Int] 
-- que ordena uma lista de inteiros em ordem crescente usando o algoritmo de 
-- ordenação por inserção. Considere na sua função que uma lista vazia já está 
-- em ordem e que para ordenar basta inserir um elemento na posição correta no 
-- restante da lista que já deve estar ordenado.
ordenaInsere :: [Int] -> [Int]
ordenaInsere []     = []
ordenaInsere (x:xs) = insere x (ordenaInsere xs)


-- 3) Defina uma função recursiva uneOrdenado :: [Int] -> [Int] -> [Int] 
-- que une duas listas já ordenadas em ordem crescente uma terceira lista 
-- que também deve estar em ordem crescente.

uneOrdenado :: [Int] -> [Int] -> [Int]
uneOrdenado [] [] = []
uneOrdenado [] ys = ys
uneOrdenado xs [] = xs
uneOrdenado (x:xs) (y:ys) | x < y     = x : (uneOrdenado xs (y:ys))
                          | otherwise = y : (uneOrdenado (x:xs) ys)

--uneOrdenado (x:xs) (y:ys) = if x < y then x : (uneOrdenado xs (y:ys)) else y : (uneOrdenado (x:xs) ys)


-- 4) Através da função uneOrdenado, defina uma função ordenaUne :: [Int] -> [Int]
-- que particiona sucessivamente uma lista na metade até atingir partições de tamanho 1
-- para então ordenar as partições através do método uneOrdenado até atingir uma lista ordenada.
-- Considere uma lista vazia e a uma lista com um elemento como ordenadas na sua definição.

ordenaUne :: [Int] -> [Int]
ordenaUne [] = []
ordenaUne xs | length xs == 1 = xs
             | otherwise      = uneOrdenado (ordenaUne metadeInf) (ordenaUne metadeSup)
                              where metadeInf = map ((!!) xs) [0.. ((length(xs) `div` 2)-1)]
                                    metadeSup = map ((!!) xs) [length(xs) `div` 2 .. length(xs)-1]

-- [3,2,1,5,4]
-- uneOrdenado ( ordenaUne [3,2,1] ) (ordenaUne [5,4]) 
-- uneOrdenado ( uneOrdenado ( ordenaUne [3,2] ) [1] ) (ordenaUne ( uneOrdenado [5] ) (uneOrdenado [4]) ) 
-- uneOrdenado ( uneOrdenado ( uneOrdenado (ordenaUne [3] ordenaUne[2]) ) [1] ) (ordenaUne [5][4] ) 
-- uneOrdenado ( uneOrdenado ( uneOrdenado [3][2] ) [1] ) [4,5] 
-- uneOrdenado ( uneOrdenado [2,3]  [1] ) [4,5] 
-- uneOrdenado [1,2,3]  [4,5] 
-- uneOrdenado [1,2,3,4,5]


-- 5) Explique a função padrão zipWith cuja definição é a seguinte:
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f (x:xs) (y:ys)=f x y : zipWith f xs ys
-- zipWith f _      _      = []

-- RESPOSTA: usando a função zipWith pode-se gerenciar ou compactar os argumentos passados 
-- em um único array, incluindo operações de adição, subtração, multiplicação, etc. 
-- Esta função permite que passemos dois valores, seguidos de uma operação, que irá 
-- compactar os valores dos argumentos, retornando um único resultado concatenado sobre os 
-- dois valores passados. 
-- Exemplo:
-- zipWith (+) [1,2,3] [3,2,1]
-- [1+3, 2+2, 3+1]
-- [4,4,4]


-- 6) A função cresc determina se uma lista está em ordem crescente:
cresc :: (Ord a) => [a] -> Bool
cresc []       = True
cresc [x]      = True
cresc (x:y:xs) = (x <= y) && cresc (y:xs)
-- Dê uma definição equivalente da função cresc usando a função zipWith.

cresc' :: (Ord a) => [a] -> Bool
cresc' [] = True
cresc' xs = and (zipWith (<=) xs (tail xs))

-- 7)Dê uma definição para a função

disjuntas :: (Ord a) => [a] -> [a] -> Bool
disjuntas [] ys = True
disjuntas xs [] = True
disjuntas (x:xs) (y:ys) | x > y = disjuntas (x:xs) ys
                        | x < y = disjuntas xs (y:ys)
                        | otherwise = False

--que recebe duas listas em ordem crescente e determina se as mesmas 
--não possuem nenhum elemento em comum, isto é, se são disjuntas.