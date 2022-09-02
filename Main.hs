module Main where

--1. Escreva uma função para o cálculo dos números da sequência de --Fibonacci, utilizando Haskell.
fibonacci :: Int -> Int
fibonacci x | x <= 1 = x
fibonacci x = fibonacci(x - 1) + fibonacci(x - 2)

--2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum (MDC) 
--de Euclides publicado por volta do ano 300 AC. Podemos simplificar este algoritmo dizendo 
--que dados dois inteiros A e B, o MDC entre eles será dado pelo valor absoluto de A se B=0 e 
--pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma função para o 
--cálculo do MDC entre dois números inteiros positivos, usando o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.

mdc :: Int -> Int -> Int
mdc a b = if b == 0 then a else mdc b (mod a b)

--3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e recursividade.
soma :: Int -> Int -> Int
soma a b = if length(show a) == 1 then a + b else soma(read(tail(show a))) b+ read[head(show a)]

--4 Escreva uma função que devolva a soma de todos os números menores que 10000 que sejam múltiplos de 3 ou 5.
soma1:: Int -> Int
soma1 x =  (3 * (x `div` 3) * (x `div` 3 + 1) `div` 2) + (5 * (x `div` 5) * (x `div` 5 + 1) `div` 2) - (15 * (x `div` 15) * (x `div` 15 + 1) `div` 2)

--5. Escreva uma função que, recebendo uma lista de inteiros, apresente a 
--diferença entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.

somaQuadrado :: [Int] -> Int
somaQuadrado x = if length x == 1 then head x ^ 2 else head x ^ 2 + somaQuadrado(tail x)
quadradoSoma :: [Int] -> Int
quadradoSoma x = if length x == 1 then head x else head x * quadradoSoma(tail x)
diferenca :: [Int] -> Int
diferenca x = ((2 * quadradoSoma x) + somaQuadrado x) - somaQuadrado x

--6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. 
--Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números 
--primos menores que um determinado inteiro dado.

divisor :: Int -> [Int]
divisor 0 = [1]
divisor n = [x | x <- [1,2..n], n `mod` x == 0]
primo :: Int -> [Int]
primo n | length(divisor n) == 2 = divisor n | otherwise = []


--8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].
aoContrario :: [a] -> [a]
aoContrario [] = []
aoContrario (x:xs) = aoContrario xs ++ [x]

--10 Escreva uma função chamada comprimento que receba uma lista de 
--inteiros e devolva o comprimento desta lista. 
--Observe que você não pode usar nenhuma função que 
--já calcule o comprimento de uma lista.
tamanhoDaLista :: [Int] -> Int
tamanhoDaLista x = sum[1 | _<- x]

main :: IO()
main = do

putStrLn $ ("Func.1: entrada 10 resultado " ++ show(fibonacci 10))
putStrLn $ ("Func.2: entrada 15; 9 resultado " ++ show(mdc 15 9))
putStrLn $ ("Func.3: entrada 1234; 0 resultado " ++ show(soma 1234 0))
putStrLn $ ("Func.4: entrada 10000 resultado " ++ show(soma1 10000))
putStrLn $ ("Func.5: entrada 1;2;3;4;5 resultado " ++ show(diferenca[1,2,3,4,5]))
putStrLn $ ("Func.6: entrada 13 resultado " ++ show(primo 13))
putStrLn $ ("Func.8: entrada 1;2;3 resultado " ++ show(aoContrario[1,2,3]))
putStrLn $ ("Func.10: entrada 1;2;3;7;8;9 resultado " ++ show(tamanhoDaLista[1,2,3,7,8,9]))
