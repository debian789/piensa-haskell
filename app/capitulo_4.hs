import Data.List
-- import Test.QuickCheck

sumaCuadradosR :: Integer -> Integer
sumaCuadradosR 0 = 0
sumaCuadradosR n = n ^2 + sumaCuadradosR (n-1)


sumarCuadradosC n = sum [x ^2 | x <- [1..n]]


numeroBloquesR :: Integer -> Integer
numeroBloquesR 0 = 0
numeroBloquesR n = 2*n + numeroBloquesR (n-1)


-- escalera2 []  = []
escalera2 :: Int -> [Int]
escalera2 0 = [0]
escalera2 n   = 1 : escalera2 (n-1)


numeroBloquesC :: Integer -> Integer
numeroBloquesC n = sum (map (*2) [1..n])

numeroBloquesC' :: Integer -> Integer
numeroBloquesC' n = sum [x*2 | x <- [1..n]]


sumaCuadradosImparesR :: Integer -> Integer
sumaCuadradosImparesR 1 = 1
sumaCuadradosImparesR n 
    | odd n = (n ^ 2) + sumaCuadradosImparesR (n-1) 
    | otherwise = sumaCuadradosImparesR (n-1) 


sumaCuadradosImparesRC :: Integer -> Integer
sumaCuadradosImparesRC n = sum [x ^ 2 | x <- [1..n], odd x]


sumaCuadradosImparesRC' :: Integer -> Integer
sumaCuadradosImparesRC' n = sum [x ^ 2 | x <- [1,3..n]]


digitosR :: Integer -> [Integer]
-- digitosR 0 = []
digitosR n 
    | n == 0 = []
    | -1 /= n = [converNumer (show (head (show n)))] ++  digitosR (converNumer (tail (show n)))
    where 
        converNumer a = read a :: Integer 


digitosRr :: Integer -> [Integer]
digitosRr n = reverse (digitosR' n)
digitosR' n
    | n < 10 = [n]
    | otherwise = (n `rem` 10) : digitosR' (n `div` 10)

numeroTexto a = 
    converNumero a
    where 
        converNumero x =  read x :: Integer

digitosC :: Integer -> [Int]
digitosC n = [read [x] | x  <- show n]



sumarDigitos :: Integer -> Integer
sumarDigitos n
    | n < 10 = n
    | otherwise = (n `rem` 10) + sumarDigitos (n `div` 10)


sumarDigitosNR :: Integer -> Integer
sumarDigitosNR n = sum [ read [x] | x <- show n ]


esDigito :: Integer -> Integer -> Bool
esDigito n a = elem (show n !! 0) (show a)

numeroDeDigitos :: Integer -> Integer
numeroDeDigitos n  = toInteger (length (show n)) 

numeroDeDigitos' :: Int -> Integer
numeroDeDigitos' n  = toInteger (length (show n))


listaNumeroR :: [Integer] -> Integer
listaNumeroR [a] = a
listaNumeroR (x:xs) = read (show x ++ show (listaNumeroR xs)) :: Integer


listaNumeroC :: [Integer] -> Integer
listaNumeroC xs = sum [y*10^n | (y,n) <- zip (reverse xs) [0..]]



pegaNumerosR :: Integer -> Integer -> Integer
pegaNumerosR x y
    | y < 10 = 10 * x +y 
    | otherwise = 10 * pegaNumerosR x (y `div` 10) + (y `rem` 10)


pegaNumerosR' :: Integer -> Integer -> Integer
pegaNumerosR' a b = read (show a ++ show b) :: Integer


primerDigitoR :: Integer -> Integer
primerDigitoR x
    | x < 10 = x 
    | otherwise = primerDigitoR (div x 10)

primerDigitoR' :: Integer -> Integer
primerDigitoR' x = 
    read nString :: Integer
    where 
        nString = [(head (show x))] :: String 

ultimoDigito :: Integer -> Integer
ultimoDigito x = 
    read nString :: Integer
    where 
        nString = [(last (show x))] :: String 

ultimoDigito' :: Integer -> Integer 
ultimoDigito' n = n `rem` 10


inverso :: Integer -> Integer
inverso x =  listaNumeroC (digitosR' x)

inverso' :: Integer -> Integer
inverso' n = read (reverse (show n ))


capicua :: Integer -> Bool
capicua n 
    | n == inverso n = True
    | otherwise = False

capicua' n = n == inverso n 



primitivo :: Integer -> Integer
primitivo n 
    | n < 10 = n 
    | otherwise =  primitivo (multiplicarItemLista (digitosRr n))


multiplicarItemLista [] = 1
multiplicarItemLista (x:xs) =  x * multiplicarItemLista xs

primitivo' :: Integer -> Integer
primitivo' n 
    | n < 10 = n
    |otherwise =  primitivo' (product (digitosRr (n)))


-- equivalentes :: Int -> Int -> Bool
equivalentes a b =
    y == x
    where
        aa = toInteger a
        aaa =(sum (digitosRr aa)) 
        lenA = length (digitosRr aa)
        x = (fromIntegral aaa /  fromIntegral lenA)

        bb = toInteger b
        bbb =(sum (digitosRr bb)) 
        lenB = length (digitosRr bb)
        y = (fromIntegral bbb /  fromIntegral lenB)



cuadradosC :: [Integer] -> [Integer]
cuadradosC xs = [ x ^ 2 | x <- xs]


cuadradosR :: [Integer] -> [Integer]
cuadradosR [] = [] 
cuadradosR (x:xs) =  [x* x] ++  cuadradosR (xs)


imparesC :: [Integer] -> [Integer]
imparesC xs = [x | x <- xs, odd x ]

imparesR :: [Integer] -> [Integer]
imparesR [] = []
imparesR (x:xs) 
    | odd x = x : imparesR (xs)
    | otherwise = imparesR (xs)

imparesCuadradosC :: [Integer] -> [Integer]
imparesCuadradosC xs =  cuadradosC (imparesC xs)


imparesCuadradosR :: [Integer] -> [Integer]
imparesCuadradosR [] = []
imparesCuadradosR (x:xs)
    | odd x = x ^ 2 : imparesCuadradosR  xs 
    | otherwise = imparesCuadradosR xs 


entreL :: Integer -> Integer -> [Integer]
entreL x y = [x..y]

entreR :: Integer -> Integer -> [Integer]
entreR x y 
    | x >= y = [y]
    | otherwise = x : entreR (x+1) y


mitadParesC :: [Int] -> [Int]
mitadParesC (x:xs) = [div s 2 | s <- (x:xs), even s ]

mitadParesR :: [Int] -> [Int]
mitadParesR [] = []
mitadParesR (x:xs) 
    | even x = (div x 2) : mitadParesR xs
    | otherwise = mitadParesR xs

enRangoC :: Int -> Int -> [Int] -> [Int]
enRangoC a b (x:xs) = [s | s <- (x:xs), s >= a && s <= b ]

enRangoC' :: Int -> Int -> [Int] -> [Int]
enRangoC' a b (x:xs) = [s | s <- (x:xs), a <= s && s <= b ]


enRangoR :: Int -> Int -> [Int] -> [Int]
enRangoR _ _ [] = [] 
enRangoR a b (x:xs) 
    | a <= x && b >= x = x : enRangoR a b xs 
    | otherwise = enRangoR a b xs 


sumaPositivosC :: [Int] -> Int
sumaPositivosC (x:xs) = sum [s | s <- (x:xs), s > 0 ]

sumaPositivosR :: [Int] -> Int
sumaPositivosR [] = 0
sumaPositivosR (x:xs) 
    | x > 0 = x + sumaPositivosR xs 
    | otherwise = sumaPositivosR xs 



aproximaPiC n = sqrt (6 * (sum [ 1 /  (x ^ 2) | x <- [1..n]  ]))


aproximaPiR 0 = 1
aproximaPiR n = sqrt (6* aproximaPiR' n )

aproximaPiR' 0 = 1
aproximaPiR' n = 1 / n ^ 2  +  aproximaPiR' (n - 1)


sustituyeImpar :: [Int] -> [Int]
sustituyeImpar  [] = []
sustituyeImpar (x:xs) 
    | even x = x : sustituyeImpar xs 
    | odd  x = x+1 : sustituyeImpar xs 


agarradoC :: [Float] -> Float
agarradoC (x:xs) = sum [s -  (s*0.10)  | s <- (x:xs), s -  (s*0.10)  <= 199 ]

agarradoR :: [Float] -> Float
agarradoR [] = 0
agarradoR (x:xs)
    | precioDescuento <= 199 = precioDescuento + agarradoR xs 
    | otherwise =   agarradoR xs  
    where 
        precioDescuento = x - (x*0.10)


factores :: Integer -> [Integer]
factores n  = [x | x <- [1..n], (mod n x) == 0 ]



primo :: Integer -> Bool
primo n = length (factores n ) == 2

primo' :: Integer -> Bool
primo' n = factores n  == [1,n]

factoresPrimos :: Integer -> [Integer]
factoresPrimos n = [x | x <- factores n , primo x ]

-- factorizacion :: Integer -> [(Integer,Integer)]
-- factorizacion n = [(x, a) | (x,a) <- factores' n , primo x ]



distanciaC :: Eq a => [a] -> [a] -> Int
distanciaC xs ys =   length [ c |  c <- [0..(menorNumero - 1 )], xs !! c /= ys !! c ]
    where 
        menorNumero = minimum [length ys, length xs] 

distanciaC' :: Eq a => [a] -> [a] -> Int
distanciaC' xs ys = length [(x,y) | (x,y) <- zip xs ys, x /= y]

distanciaR :: Eq a => [a] -> [a] -> Int
distanciaR []  _ = 0
distanciaR _  [] = 0
distanciaR (x:xs) (y:ys) 
    | x /= y = 1 + distanciaR xs ys 
    | otherwise = distanciaR xs ys 

traspuesta :: [[a]] -> [[a]]
traspuesta  [[],[]] = []
traspuesta [(x:xs),(y:ys)]
    | length (x:xs) == 3 = [x,y] : traspuesta [xs ,ys] 
    | otherwise = traspuesta [xs,  ys] 

traspuesta' :: [[a]] -> [[a]]
traspuesta'  (xs)  
    | length (xs) == 2 = [[x,y] | (x,y) <-  zip (xs !! 0)  (xs !! 1)]
    | length (xs) == 3 = [[x,y,z]  | (x,y,z) <-  zip3 (head xs) (xs !! 1) (last xs)] 

traspuesta'' :: [[a]] -> [[a]]
traspuesta'' [] = []
traspuesta'' ([]:xss) = traspuesta'' xss
traspuesta'' ((x:xs):xss) =
    (x:[h | (h:_) <- xss]) : traspuesta'' (xs : [t | (_:t) <- xss])


