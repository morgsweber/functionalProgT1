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
uneOrdenado [] _ = []
uneOrdenado _ [] = []
uneOrdenado (x:xs) (y:ys) = if x < y then uneOrdenado 


-- 4) Através da função uneOrdenado, defina uma função ordenaUne :: [Int] -> [Int]
-- que particiona sucessivamente uma lista na metade até atingir partições de tamanho 1
-- para então ordenar as partições através do método uneOrdenado até atingir uma lista ordenada.
-- Considere uma lista vazia e a uma lista com um elemento como ordenadas na sua definição.

--ordenaUne :: [Int] -> [Int]




