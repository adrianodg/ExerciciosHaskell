--Escrever uma função que encontre as raízes reais de uma equação do 2o grau

raiz :: Float -> Float -> Float -> [Float]
raiz a b c = [ menosb2a - raizquadelta, menosb2a + raizquadelta ]
  where menosb2a = -b / (2 * a)
        raizquadelta = (sqrt (b*b - 4 * a * c)) / (2 * a)


{-Fornecidos três valores diferentes entre si, a, b e c, elaborar uma função que retorne
quantos desses três números são maiores que o valor médio entre eles.
-}

maioresMedia :: Float -> Float -> Float -> Float
maioresMedia a b c
  | a == b && b == c = 0
  | (a > media) && (b > media) = 2
  | (a > media) && (c > media) = 2
  | (b > media) && (c > media) = 2
  | (a > media) = 1
  | (b > media) = 1
  | (c > media) = 1
  where media = ((a + b + c) / 3)


{-Calcular a soma dos números inteiros compreendidos em um intervalo [x,y], incluindo e
excluindo os limites.
somaIntervalo :: Int -> Int -> (Int, Int)​ , onde é produzido uma tupla com
o primeiro valor refere-se à soma incluindo os limites e o segundo excluindo os limites.
-}

somaIntervalo :: Int -> Int -> (Int, Int)
somaIntervalo a b = ((sum (geraLista a b)), sum (geraListaMenor a b))

geraLista :: Int -> Int -> [Int]
geraLista 0 0 = []
geraLista a b = [a..b]

geraListaMenor :: Int -> Int -> [Int]
geraListaMenor 0 0 = []
geraListaMenor 0 b = [0..b-1]
geraListaMenor 1 b = [2..b-1]
geraListaMenor a b = [a+1..b-1]

somaRec :: [Int] -> Int
somaRec [a] = 0
somaRec (x:xs) = x + somaRec xs

aux :: [Int] -> [Int]
aux (x:xs) = xs

som :: Int -> Int -> Int
som a b = somaRec (aux (geraLista a b))


{-Construa uma função que retorne o MMC (Mínimo Múltiplo Comum) entre dois números
inteiros.
-}

mdc :: Int -> Int -> Int
mdc m n
  | m == 0 = n
  | m > 0 = mdc (n `mod` m) m
  
mmc :: Int -> Int -> Int
mmc _ 0 = 0
mmc 0 _ = 0
mmc x y = (x `div` mdc x y) * y


{-Construa uma função que retorne o MMC (Mínimo Múltiplo Comum) de uma lista de
números inteiros.
-}

mdc :: Int -> Int -> Int
mdc m n
  | m == 0 = n
  | m > 0 = mdc (n `mod` m) m
  
mmc :: Int -> Int -> Int
mmc _ 0 = 0
mmc 0 _ = 0
mmc x y = (x `div` mdc x y) * y

mmcLista :: [Int] -> Int
mmcLista [] = 1
mmcLista (h:t) = mmc h (mmcLista t)


--Construa uma função que retorne o Máximo Divisor Comum entre dois números inteiros.

mdc :: Int -> Int -> Int
mdc m n
  | m == 0 = n
  | m > 0 = mdc (n `mod` m) m


{-Construa uma função que retorne o Máximo Divisor Comum de uma lista de números
inteiros.
-}

mdc :: Int -> Int -> Int
mdc m n
  | m == 0 = n
  | m > 0 = mdc (n `mod` m) m
  
mmc :: Int -> Int -> Int
mmc _ 0 = 0
mmc 0 _ = 0
mmc x y = (x `div` mdc x y) * y

mdcLista :: [Int] -> Int
mdcLista [] = 0
mdcLista (h:t) = mdc h (mdcLista t)


{-Defina uma função recursiva que recebe dois inteiros m e n, onde m < n, e retorna o
produto de todos os números no intervalo [m,n].
-}

produto :: Int -> Int -> Int

produto a b
  | a == 0 = 0
  | b == 0 = 0
  | otherwise = (product [a..b] )


{-A raiz quadrada inteira de um número inteiro positivo n é o maior número inteiro cujo
quadrado é menor ou igual a n. Por exemplo, a raiz quadrada inteira de 15 é 3, e a raiz
quadrada inteira de 16 é 4. Defina uma função recursiva para calcular a raiz quadrada
inteira.
-}

raiz :: Int -> Int
raiz a = raizAux a 1

raizAux :: Int -> Int -> Int
raizAux a b
  | a == b^2 = b
  | a < b^2 = (b-1)
  | a > b^2 = raizAux a (b+1)


{-Elaborar uma função para concatenar duas listas, sem utilizar o operador de
concatenação do Haskell (++).
-}

conc :: [Int] -> [Int] -> [Int]
conc [] [] = []
conc [] a = a
conc a [] = a
conc (h:t) b = h: conc t b


{-Defina uma função que dada uma lista de números reais A, retorne uma lista B com os
elementos presentes em A que são menores do que a média de todos os elementos de A.
-}

menoresQueMedia :: (Num a) => [a] -> [a]
menoresQueMedia a = menoresQueMediaAux a

menoresQueMediaAux :: (Num b) => [b] -> [b]
menoresQueMediaAux [] = []
menoresQueMediaAux (h : t)
  | h < media a = h:(menoresQueMediaAux t)
  | h > media a = menoresQueMediaAux t

media :: [Float] -> Float
media [] = 0
media a = (sum a / Float(length a))

