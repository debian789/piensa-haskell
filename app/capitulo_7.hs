import Data.List
-- repite :: a -> [a]
repite' n = [n,n..]
-- take 4 (repite 4)

repite n = [n] ++ repite n
-- take 4 (repite 4)

repiteC n = [n | _ <- [1..]]
-- take 4 (repite 4)

repiteFinita :: Int-> a -> [a]
repiteFinita 0 _ = []
repiteFinita n a = a : repiteFinita (n-1) a

repiteFinitaC :: Int-> a -> [a]
repiteFinitaC n a = [a | _ <- [1..n]]

repiteFinita' n a = take n (repite a)

-- ecoC :: [a] -> [a]
-- ecoC xs = foldl (\ x y -> x++ [y | j <- [0..(length x)]] ) [] xs 
ecoC  :: [a] -> [a]
ecoC xs = concat ([ repiteFinitaC y x | (x,y) <- zip xs [1..] ])

ecoC1 :: [a] -> [a]
ecoC1 xs = foldl (\x y -> x++y) [] [ repiteFinitaC y x | (x,y) <- zip xs [1..]]

ecoC2 xs =  concat (reverse (foldl (\x y -> ( repiteFinitaC (1+length x) y): x  ) [] xs))   --  foldl (\x y -> x++y) [] [ repiteFinitaC y x | (x,y) <- zip xs [1..]]  




ecoR :: [a] -> [a]
ecoR (y:ys) =
    ecoR'2 1 (y:ys)
    where
        ecoR'2 _ [] = []
        ecoR'2 n (x:xs) = (repiteFinitaC (n) x) ++ ecoR'2 (n+1) xs



potenciasMenores :: Int -> Int -> [Int]
potenciasMenores n a = takeWhile (<a) (map (\ x -> 2 ^ x) [1..])



itera :: (a -> a) -> a -> [a]
itera f n =  [n] ++ itera f  (f n)

agrupa :: Int -> [a] -> [[a]]
agrupa n [] = []
agrupa n (x:xs) = [ y | y <-   take n (x:xs) ] : agrupa n (drop  n (x:xs))


agrupa' :: Int -> [a] -> [[a]]
agrupa' n =
    takeWhile (not . null)
    . map (take n)
    . iterate (drop n)

siguiente :: Integer -> Integer
siguiente n
    | n == 1 = 1
    | even n = div n  2
    | otherwise = (n * 3) + 1


collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n =  n : collatz (siguiente n)

collatz' n = takeWhile (/=1) (iterate siguiente n) ++ [1]
-- collatz' 13               [13,40,20,10,5,16,8,4,2,1]

menorCollatzMayor :: Int -> Integer
menorCollatzMayor n = head [y | y <- [1..], length (collatz y) > n]
-- menorCollatzMayor 100     27  



factores :: Integer -> [Integer]
factores n  = [x | x <- [1..n], (mod n x) == 0 ]


primo :: Integer -> Bool
primo n = factores n  == [1,n]


primos = [x | x <- [1..], primo x]

-- primo' :: Integral a => a -> Bool
primo' x = x == head (dropWhile (<x) primos)


-- sumaDeDosPrimos :: Int -> [(Int,Int)]
sumaDeDosPrimos n =
    [p | j <- listadoPrimos, p <-  recorrer j listadoPrimos n ]
    where
        listadoPrimos = takeWhile  (<n) primos
        recorrer _ [] _ = []
        recorrer a (x:xs) c
            | (a+x) == c && a < x = [(a,x)] ++  recorrer a xs c
            | otherwise  = recorrer a xs c

esProductoDeDosPrimos :: Integer -> Bool
esProductoDeDosPrimos n
    | length (factores n) > 3 = True
    | otherwise = False


esMuyCompuesto :: Integer -> Bool
esMuyCompuesto n = (length (factores n) >= 6)
-- esMuyCompuesto 25     False 
-- esMuyCompuesto 24     True

muyCompuesto :: Int -> Integer
muyCompuesto n = [x | x <- [1..], esMuyCompuesto x ] !! n


-- primoTruncable :: Int -> Bool
-- primoTruncable 
primoTruncable n
    | n < 10 = primo n
    | not (primo n) = False
    | otherwise = primoTruncable (div n 10)

-- primoPermutable :: Int -> Bool




primoPermutable :: Integer -> Bool
primoPermutable n  = primoPermutable' n  n



primoPermutable' :: Integer -> Integer -> Bool
primoPermutable' n  m 
    | not (primo n)  = False
    | n == m      = primoPermutable' (voltearNumeros n) m
    | n /= m  && primo  n        = True



voltearNumeros n  =
    numberVolteado
    where 
        sizeNumber = length (show n)
        lastNumber = n - (10 * (div n 10))
        numberWioutLast = div n 10 
        numberVolteado = ((10 ^ (sizeNumber -1)) * lastNumber ) + numberWioutLast


-- enteros :: [Int]
enteros' = foldl (\x y -> x++[negate y, y]  ) [0] [1..]

enteros'' = concat [ [negate x] ++ [x] | x <- [0..10]]

enteros =  concat (map (\x -> [negate x] ++ [x]  )  [0..])



-- sumaPrimoMenores :: Int -> Int
sumaPrimoMenores n =  sum(takeWhile (<n) primos)



-- euler12 :: Int -> Integer
euler12 n =     
    ba da
    where 
        triangulares = [sum [1..x] |  x  <- [1..] ]
        da           =  [ (factores x, x ) | x  <- triangulares ]
        ba (x:xs)
            | length (fst x) > n     = snd x
            | otherwise = ba xs

-- euler12 5       28



paresOrdenados :: [a] -> [(a,a)]
paresOrdenados [] = []
paresOrdenados (x:xs) = 
    concat( paresOrdenados' (x:xs))
    where 
        listadoData _ [] = []
        listadoData a (y:ys) = (a,y) : listadoData a ys 
        paresOrdenados' [] = []
        paresOrdenados' (x:xs) = listadoData x (xs) : paresOrdenados' xs
-- paresOrdenados [3,2,5,4]             [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]


paresOrdenadosR [] = []
paresOrdenadosR (x:xs) = foldr (\y  ac -> (x,y):ac) (paresOrdenadosR xs) xs
-- paresOrdenadosR [3,2,5,4]             [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]


paresOrdenados3 [] = []
paresOrdenados3 (x:xs) =  [ (d,y) |  (y, d)  <-  zip  xs (repeat x) ] ++ paresOrdenados3 xs
-- paresOrdenados3 [3,2,5,4]             [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]

paresOrdenados4 [] = []
paresOrdenados4 (x:xs) = zip (repeat x) xs ++ paresOrdenados4 xs
-- paresOrdenados4 [3,2,5,4]             [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]


potenciaFunc :: Int -> (a -> a) -> a -> a
potenciaFunc 0 f a =  a 
potenciaFunc n f a =  potenciaFunc (n-1) f (f a) 
-- potenciaFunc 4 (+10) 5                 45

potenciaFunc2 :: Int -> (t -> t) -> t -> t
potenciaFunc2 n f a = foldl1 (\x y -> f x  ) (take (n+1)  (repeat a))
-- potenciaFunc2 3 (*10) 5                5000

potenciaFunc3 :: Int -> (a -> a) -> a -> a
potenciaFunc3 n f x = last (take (n+1) (iterate f x))
-- potenciaFunc3 3 (*10) 5                5000

sumaDeDos :: Int -> [Int] -> Maybe (Int,Int)
sumaDeDos _ [] = Nothing 
sumaDeDos _ [_] = Nothing  
sumaDeDos y (x:xs) 
    | y-x `elem` xs = Just (x, y-x)
    | otherwise = sumaDeDos y xs 
-- sumaDeDos 9 [7,4,6,2,5]                 Just (7,2)



-- sumaDeDos' :: Int -> [Int] -> Maybe (Int,Int)
sumaDeDos' n (x:xs) 
    | null sy = Nothing 
    | otherwise = Just (head sy)
    where 
        sy =  [ (x,y) |  (x,y) <- paresOrdenados (x:xs), x+y == n]


-- golomb :: Int -> Int
golomb1 n = [take x (repeat x) | x <- [1..]]

golomb n = 
    head [ x | (x,y) <- zip data4 [1..], y == n]
    where 
        data2 = [ take 2 (repeat x) | x <- [2..]]
        data3 []     _ = []
        data3 (x:xs) n =  repiteFinita (x !! 0) n ++ repiteFinita (x !! 1) (n+1)  ++  data3  xs (n+2)
        data4 = [1]++ data3 data2 2
-- golomb 9             5

sucGolomb  =  data4
    where 
        data2 = [ take 2 (repeat x) | x <- [2..]]
        data3 []     _ = []
        data3 (x:xs) n =  repiteFinita (x !! 0) n ++ repiteFinita (x !! 1) (n+1)  ++  data3  xs (n+2)
        data4 = [1]++ data3 data2 2
-- take 15 sucGolomb      [1,2,2,3,3,4,4,4,5,5,5,6,6,6,6]


subSucGolomb n   =  
    showItem sucGolomb n
    where 
        showItem (x:xs) j 
            | j > x = showItem xs j
            | j == x = x : showItem xs j
            | otherwise  = x : showItem xs j
-- take 10 (subSucGolomb 4)           [4,4,4,5,5,5,6,6,6,6]