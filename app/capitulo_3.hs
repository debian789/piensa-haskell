-- cabal install QuickCheck
-- o
-- stack install QuickCheck
import Test.QuickCheck


potencia _ 0 = 1
potencia x a =  x * potencia x (a - 1)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x a = a : replicate (x - 1) a

-- 3.3 Doble factorial
dobleFactorial 0 = 1
dobleFactorial 1 = 1
dobleFactorial n = n * dobleFactorial (n -2)

-- 3.4. Algoritmo de Euclides del máximo común divisor
mcd a 0 = a  
mcd a b = mcd b ( mod a b )


menorDivisible a b 
    | a == b  = a 
    | otherwise = lcm a (menorDivisible (a+1) b)


elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) 
    | a /= x = elem' a xs 
    | otherwise = True


last' :: [a] -> a
last' [a] =  a
last' (x:xs) = last' (xs)


concat' :: [[a]] -> [a]
concat' [] = [] 
concat' (x:xs) =  x ++ concat' xs

selecciona :: [a] -> Int -> a
selecciona  (x:xs) 0 = x
selecciona (x:xs) a = selecciona xs (a-1) 


take' :: Int -> [a] -> [a]
take' 0 xs = []
take' _ [] = []
take' a (x:xs) = x : take' (a-1) xs

refinada :: [Float] -> [Float]
refinada (x:y:zs) = x : (x+y) / 2 : refinada (y:zs)
refinada xs = xs 

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) (y:ys) 
    | x <= y    = x : mezcla xs (y:ys)
    | otherwise = y : mezcla (x:xs) ys 



mezcla2 [a] = [a]
mezcla2 [] = []
mezcla2 (x:y:xs) 
   | x > y = x : mezcla2 (y:xs)
   | otherwise =  mezcla2 (x:xs) ++ [y]
    -- | y > x = y : mezcla2 (y:xs)

-- 3.14.2 Mitades de un alista
mitades :: [a] -> ([a],[a])
mitades xs = 
    ((take leng xs), (drop leng xs) )
    where 
        leng = div (length xs) 2

mitades' :: [a] -> ([a],[a])
mitades' xs = splitAt (length xs `div` 2) xs 


ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = 
    mezcla (ordMezcla ys) (ordMezcla zs)
    where 
        (ys, zs) = mitades xs 


ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs)
    | x < y = ordenada (y:xs)
    | otherwise = False

-- 3.14.5 La ordenacion por mezcla de una permutacion 
borrar n [] = []
borrar n (x:xs)
    | n == x    = borrar 0 (xs)
    | otherwise = x: borrar n (xs)

