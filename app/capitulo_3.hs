-- cabal install QuickCheck
-- o
-- stack install QuickCheck
import Test.QuickCheck

-- 3.1. Potencia de exponente natural
potencia _ 0 = 1
potencia x a =  x * potencia x (a - 1)
-- R: potencia 2 4                       16 


-- 3.2. Replicación de un elemento
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x a = a : replicate (x - 1) a
-- R: replicate' 5 3                     [3,3,3,3,3]


-- 3.3 Doble factorial
dobleFactorial 0 = 1
dobleFactorial 1 = 1
dobleFactorial n = n * dobleFactorial (n -2)
-- R: dobleFactorial 5                   15 


-- 3.4. Algoritmo de Euclides del máximo común divisor
mcd a 0 = a  
mcd a b = mcd b ( mod a b )
-- R: mcd 6 12                           6 


-- 3.5. Menor número divisible por una sucesión de números
menorDivisible a b 
    | a == b  = a 
    | otherwise = lcm a (menorDivisible (a+1) b)
-- R: menorDivisible 10 12                660 


-- 3.8. Pertenencia a una lista
elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) 
    | a /= x = elem' a xs 
    | otherwise = True
-- R: elem' 2 [23,4,2]                    True 


-- 3.9. Último elemento de una lista
last' :: [a] -> a
last' [a] =  a
last' (x:xs) = last' (xs)
-- R: last' [2,3,4]                        4


-- 3.10. Concatenación de una lista
concat' :: [[a]] -> [a]
concat' [] = [] 
concat' (x:xs) =  x ++ concat' xs
-- R: concat' [[1],[3,4,5]]                [1,3,4,5] 


-- 3.11. Selección de un elemento
selecciona :: [a] -> Int -> a
selecciona  (x:xs) 0 = x
selecciona (x:xs) a = selecciona xs (a-1) 
-- R: selecciona [1,2,3,4] 2                3


-- 3.12. Selección de los primeros elementos
take' :: Int -> [a] -> [a]
take' 0 xs = []
take' _ [] = []
take' a (x:xs) = x : take' (a-1) xs
-- R: take' 3 [1,2,3,4]                     [1,2,3] 


-- 3.13. Intercalación de la media aritmética
refinada :: [Float] -> [Float]
refinada (x:y:zs) = x : (x+y) / 2 : refinada (y:zs)
refinada xs = xs 
-- R: refinada [1,2,3,4]                     [1.0,1.5,2.0,2.5,3.0,3.5,4.0] 


-- 3.14. Ordenación por mezcla
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) (y:ys) 
    | x <= y    = x : mezcla xs (y:ys)
    | otherwise = y : mezcla (x:xs) ys 
-- R:  mezcla [1,2,3] [4,5,6]                 [1,2,3,4,5,6] 


-- 3.14. Ordenación por mezcla
mezcla2 [a] = [a]
mezcla2 [] = []
mezcla2 (x:y:xs) 
   | x > y = x : mezcla2 (y:xs)
   | otherwise =  mezcla2 (x:xs) ++ [y]
-- R: mezcla2 [1,2,3,4,5]                      [1,5,4,3,2] 


-- 3.14.2 Mitades de un alista
mitades :: [a] -> ([a],[a])
mitades xs = 
    ((take leng xs), (drop leng xs) )
    where 
        leng = div (length xs) 2
-- R: mitades [1,2,3,4,5]                       ([1,2],[3,4,5])


-- 3.14.2. Mitades de una lista
mitades' :: [a] -> ([a],[a])
mitades' xs = splitAt (length xs `div` 2) xs 
-- R: mitades' [0,1,2,3,4,5]                    ([0,1,2],[3,4,5]) 


-- 3.14.3. Ordenación por mezcla
ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = 
    mezcla (ordMezcla ys) (ordMezcla zs)
    where 
        (ys, zs) = mitades xs 
-- R: ordMezcla  [5,2,1,3,4]                     [1,2,3,4,5] 


-- 3.14.4. La ordenación por mezcla da listas ordenadas
ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs)
    | x < y = ordenada (y:xs)
    | otherwise = False
-- R: ordenada  [0,1,2,3,4,5]                     True 


-- 3.14.5 La ordenacion por mezcla de una permutacion 
borrar n [] = []
borrar n (x:xs)
    | n == x    = borrar 0 (xs)
    | otherwise = x: borrar n (xs)
-- R: borrar 4 [1,2,3,4,5]                         [1,2,3,5] 
