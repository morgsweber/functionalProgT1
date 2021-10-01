-- 98703-02 - Programação Funcional - Turma 010 - 2021/2 - Prof. Diego Vrague Noble
-- Nomes: Arthur Musskopf Luz, Daniela Pereira Rigoli, Morgana Luiza Weber

-- 1) Defina uma função recursiva insere :: Int -> [Int] -> [Int] 
-- que insere um inteiro na posição correta em uma lista de inteiros já ordenada. 
-- Mostre o passo-a-passo da aplicação de insere 3 [1,2,4,5]
insere :: Int -> [Int] -> [Int]
insere n []                  = n : []
insere n (x:xs) | n <= x     = n : x : xs
                | otherwise  = x : (insere n xs)

-- Exemplo de passo a passo:
-- insere 3 [1,2,4,5]
-- []
-- (3 <= 1) [1] 
-- (3 <= 2) [1,2]
-- (3 <= 3) [1,2,3] ++ [4,5,6]
-- (3 <= 4) [1,2,3,4,5,6]

-- Descrição:
-- Essa função insere um inteiro considerando a ordem em uma lista de inteiros já ordenada
-- A função vai verificando se o valor a ser inserido é menor ou igual a cada elemento da lista até
-- encontrar a posição na qual o novo valor deve ficar. Fazendo uma concatenação dos elementos menos 
-- ou iguais ao valor inserido com o valor inserido e o restante da lista.


-- 2) Usando a função insere, defina a função ordenaInsere :: [Int] -> [Int] 
-- que ordena uma lista de inteiros em ordem crescente usando o algoritmo de 
-- ordenação por inserção. Considere na sua função que uma lista vazia já está 
-- em ordem e que para ordenar basta inserir um elemento na posição correta no 
-- restante da lista que já deve estar ordenado.
ordenaInsere :: [Int] -> [Int]
ordenaInsere []     = []
ordenaInsere (x:xs) = insere x (ordenaInsere xs)

-- Passo a passo: 
-- ordenaInsere [3,2,1]
-- insere 3 (ordenasInsere [2,1])
-- insere 3 (insere 2 (insere 1 (ordenaInsere [])) ) 
-- insere 3 (insere 2 (insere 1 []))
-- insere 3 (insere 2 [1]) 
-- insere 3 [1,2]
-- [1,2,3]

-- Descrição:
-- Ordena uma lista de inteiros em ordem crescente usando o algoritmo de 
-- ordenação por inserção, assim a função vai inserindo cada elemento da lista


-- 3) Defina uma função recursiva uneOrdenado :: [Int] -> [Int] -> [Int] 
-- que une duas listas já ordenadas em ordem crescente uma terceira lista 
-- que também deve estar em ordem crescente.

uneOrdenado :: [Int] -> [Int] -> [Int]
uneOrdenado [] [] = []
uneOrdenado [] ys = ys
uneOrdenado xs [] = xs
uneOrdenado (x:xs) (y:ys) | x < y     = x : (uneOrdenado xs (y:ys))
                          | otherwise = y : (uneOrdenado (x:xs) ys)

-- Passo a passo:
-- uneOrdenado [1,4] [2,3]
-- uneOrdenado ()

-- Descrição:
-- O uneOrdenado vai comparando cada elemento das duas listas, x e y, que recebe
-- e caso x seja maior que y então passa x concatenado com o método uneOrdenado (recurssão) passando 
-- como parametro o restante da lista de x e o elemento y com o restante de sua lista.
-- caso contrario, y será concatenado com a recurssão que receberá como parametro
-- x com a lista de x e o restante da lista de y.
-- Isso ocorre para que nenhum elemento seja perdido durante a união ordenada.


-- 4) Através da função uneOrdenado, defina uma função ordenaUne :: [Int] -> [Int]
-- que particiona sucessivamente uma lista na metade até atingir partições de tamanho 1
-- para então ordenar aquias partições através do método uneOrdenado até atingir uma lista ordenada.
-- Considere uma lista vazia e a uma lista com um elemento como ordenadas na sua definição.

ordenaUne :: [Int] -> [Int]
ordenaUne [] = []
ordenaUne xs | length xs == 1 = xs
             | otherwise      = uneOrdenado (ordenaUne metadeInf) (ordenaUne metadeSup)
                              where metadeInf = map ((!!) xs) [0.. ((length(xs) `div` 2)-1)]
                                    metadeSup = map ((!!) xs) [length(xs) `div` 2 .. length(xs)-1]

-- Passo a passo: 
-- [3,2,1,5,4]
-- uneOrdenado ( ordenaUne [3,2,1] ) (ordenaUne [5,4]) 
-- uneOrdenado ( uneOrdenado ( ordenaUne [3,2] ) [1] ) (ordenaUne ( uneOrdenado [5] ) (uneOrdenado [4]) ) 
-- uneOrdenado ( uneOrdenado ( uneOrdenado (ordenaUne [3] ordenaUne[2]) ) [1] ) (ordenaUne [5][4] ) 
-- uneOrdenado ( uneOrdenado ( uneOrdenado [3][2] ) [1] ) [4,5] 
-- uneOrdenado ( uneOrdenado [2,3]  [1] ) [4,5] 
-- uneOrdenado [1,2,3]  [4,5] 
-- uneOrdenado [1,2,3,4,5]

-- Descrição:
-- O método ordenaUne recebe uma lista de inteiros, esta lista é particionada em duas partes
-- sucessivamente enquanto chama o método uneOrdenado, até que tenha apenas um elemento na lista. 
-- Cada vez que o ordenaUne é chamado recursivamente, os elementos vão sendo reenseridos na lista 
-- de forma ordenada.


-- 5) Explique a função padrão zipWith cuja definição é a seguinte:
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f (x:xs) (y:ys)=f x y : zipWith f xs ys
-- zipWith f _      _      = []

-- RESPOSTA: a função zipWith recebe três argumentos, uma função, e duas listas. A função recebida por parâmetro
-- é aplicada sobre as listas de forma que o resultado seja 'compactado' e o retorno seja apenas uma lista.
-- Passo a passo:
-- zipWith (+) [1,2,3] [3,2,1]
-- [1+3, 2+2, 3+1]
-- [4,4,4]


-- 6) A função cresc determina se uma lista está em ordem crescente:
-- cresc :: (Ord a) => [a] -> Bool
-- cresc []       = True
-- cresc [x]      = True
-- cresc (x:y:xs) = (x <= y) && cresc (y:xs)
-- Dê uma definição equivalente da função cresc usando a função zipWith.

cresc' :: (Ord a) => [a] -> Bool
cresc' [] = True
cresc' xs = and (zipWith (<=) xs (tail xs))

-- Passo a passo:
-- cresc' [1,2,3]
-- and (zipWith (<=) [1,2,3] (tail [1,2,3]) )
-- and (zipWith (<=) [1,2,3] [2,3])
-- and (zipWith (<=) [1,2,3] [2,3])
-- and ((<=) [(1,2),(2,3)])
-- and ([(1 <= 2), (2 <= 3)])
-- and ([True, True])
-- True

-- Descrição: 
-- A função cresc' recebe uma lista de ordenáveis e determina se os elementos estão em ordem.
-- A função cresc' também recebe uma lista de ordenáveis, porém utiliza da função zipWith para 
-- determinar se está ordenada ou não. Esta função pega todos itens de uma lista exceto o primeiro 
-- elemento e faz a junção em pares em tuplas crescentes e aplica a função de comparação (<=) para 
-- todos as tuplas geradas.


-- 7)Dê uma definição para a função
-- disjuntas :: (Ord a) => [a] -> [a] -> Bool
-- que recebe duas listas em ordem crescente e determina se as mesmas 
-- não possuem nenhum elemento em comum, isto é, se são disjuntas.

disjuntas :: (Ord a) => [a] -> [a] -> Bool
disjuntas [] ys = True
disjuntas xs [] = True
disjuntas (x:xs) (y:ys) | x > y = disjuntas (x:xs) ys
                        | x < y = disjuntas xs (y:ys)
                        | otherwise = False

-- Passo a passo:
--         disjuntas [1,2,3] [3,4,5]
-- (1 < 3) disjuntas [2,3] [3,2,1]
-- (2 < 3) disjuntas [3] [3,2,1]
-- (otherwise) False
-- False

-- Descrição:
-- A função disjuntas é recursiva e recebe duas listas já ordenadas, como sabemos essa informação,
-- comparamos os dois elementos da 1° posição de cada uma das listas, (os nomearemos X e Y),
-- se X for maior que Y, então chamamos recursivamente disjuntas com a primeira lista completa e
-- a segunda lista é enviada sem o elemento, visto que a comparação nos garante que ele não é repetido nas 2 listas.
-- e assim é enviado até não restar mais nenhum.
