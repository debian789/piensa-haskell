import Data.Char
import Data.List

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x : takeWhile' f xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = x : dropWhile' f xs

-- concatR [[1,3],[2,4,6],[1,9]] == [1,3,2,4,6,1,9]

concatR [] = []
concatR (x:xs) = x ++ concatR xs

concatP xs = foldr  (++) [] xs

divideMedia (x:xs) =
    (mediaMenores (x:xs), mediaMayores (x:xs))
    where
        mediaLista xa = (sum xa) `div` (length xa)
        mediaMenores ys = [ y | y <- ys, mediaLista ys > y ]
        mediaMayores ys = [ y | y <- ys, mediaLista ys < y ]

divideMedia' (x:xs) =
    (mediaMenores (x:xs), mediaMayores (x:xs))
    where
        mediaLista xa = (sum xa) `div` (length xa)
        mediaMenores ys = filter (< (mediaLista ys)) ys
        mediaMayores ys = filter (> (mediaLista ys)) ys


segmetos :: (a -> Bool) -> [a] -> [[a]]
segmetos _ [] = []
segmetos p xs =
    takeWhile p xs : (segmetos p (dropWhile (not.p) (dropWhile p xs)))


relacionados :: (a -> a -> Bool) -> [a] -> Bool
relacionados r xs = and [r x y | (x,y) <- zip xs (tail xs)]

-- agrupa :: Eq a => [[a]] -> [[a]]
-- agrupa (x:xs) = [head x] ++ agrupa xs
-- agrupa [] = []
agrupa [] = []
agrupa (z:bz)
    | elem [] bz = []
    | otherwise = [agrupa2 (z:bz) ] ++ agrupa (clearFirstItem (z:bz))
    where
        itemX xs = head xs
        agrupa2 [] = []
        agrupa2 (z:xz) = itemX z : agrupa2 xz
        clearFirstItem [] = []
        clearFirstItem (z:xz) = drop 1 z : clearFirstItem xz



        -- itemXa xy = [head (itemX xy)]
        -- listItem ax = itemXa ax 

agrupa' :: Eq a => [[a]] -> [[a]]
agrupa' [] = []
agrupa' xss
    | [] `elem` xss = []
    | otherwise = primeros xss : agrupa' (restos xss)
    where
        primeros = map head
        restos = map tail

superpar :: Int -> Bool
superpar 0 = False
superpar n
    | n < 10 = even n
    | otherwise = even n && superpar (div n 10)


superparC :: Int -> Bool
superparC n
    | elem False nl = False
    | otherwise = True
    where
        nT = show n
        nl = [ even (read [x] :: Int) | x <- nT]

superparAll n = all even [ (read [x] :: Int) | x <- (show n)]


superpar5 :: Int -> Bool
superpar5 n = filter even ([ (read [x] :: Int) | x <- (show n)]) == [ (read [x] :: Int) | x <- (show n)]


-- filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- filtraAplica :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filtraAplicaC f1 f2 xs =  [ f1 y | y <-[ x | x <- xs, f2 x]]

filtraAplicaM f p xs = map f (filter p xs)

filtraAplicaR _ _ [] = []
filtraAplicaR f p (x:xs)
    | p x = f x : filtraAplicaR f p xs
    | otherwise = filtraAplicaR f p xs


filtraAplica_4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_4 f p =
    foldr g []
    where
        g x y
            | p x = f x : y
            | otherwise = y

filtraAplica_4' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_4' f p =
    foldr (\x y -> if p x then (f x : y) else y) []


-- maximumR :: Ord a => [a] -> a


maximumR ys =
    maximumR' ys !! 0
    where
        maximumR' [a] = [a]
        maximumR' [] = []
        maximumR' (x:xs)
            | x > head (xs) =maximumR' (x :tail(xs))
            | head xs > x = maximumR' (xs)
            | otherwise = (x:xs)


maximumR2 :: Ord a => [a] -> a
maximumR2 [x] = x
maximumR2 (x:y:ys) = max x (maximumR2 (y:ys))


-- maximumP :: Ord a => [a] -> a
-- maximumP :: t a -> a
maximumP xs = foldr1 (\x y -> if x  > y then x else y ) xs

maximumP' :: Ord a => [a] -> a
maximumP' = foldr1 max


-- minimunP'2 :: Ord a => [a] -> a
minimumP xs = foldr1 min xs

inversaR :: [a] -> [a]
inversaR [] = []
inversaR (x:xs) = inversaR xs ++ [x]

-- inversaP :: [a] -> [a]
inversaP xs = foldr (\x y ->  y ++ [x] ) [] xs


inversaP' xs = foldl (\x y ->  [y] ++ x ) [] xs


-- dec2entR :: [Int] -> [Int]
dec2entR ys =
    numberInt (reverse ys) 1
    where
        numberInt [] _ = 0
        numberInt (x:xs) n = numberInt xs (n*10) + (x*n)



-- dec2entP :: (Foldable t, Num a1) => t a2 -> a1
dec2entP ys = foldl (\x y -> (x * 10)+y ) 0 ys


sumaR :: Num b => (a -> b) -> [a] -> b
sumaR f []     = 0
sumaR f (x:xs) =  (f x) +  sumaR f xs



sumaP :: Num b => (a -> b) -> [a] -> b
sumaP f xs = foldl (\x y -> (f y) + x) 0 xs


mapR :: (t -> a) -> [t] -> [a]
mapR f [] = []
mapR f (x:xs) = f x : mapR f xs 


mapP :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
mapP f xs = foldr (\ x y -> f x :  y ) [] xs

mapP'' :: (a -> b) -> [a] -> [b]
mapP'' f = foldr ((:) . f) []

filterR f [] = []
filterR f (x:xs) 
    | f x  = x : filterR f xs
    | otherwise = filterR f xs 


filterP :: Foldable t => (a -> Bool) -> t a -> [a]
filterP f xs = foldr (\ x y -> if f x then  x:y else y  ) [] xs


sumllR :: Num a => [[a]] -> a
sumllR zs = 
    sumllR'2 zs
    where 
        sumllR' [a] = a
        sumllR' (x:xs) = x + sumllR' xs 

        sumllR'2 [a] = sumllR' a
        sumllR'2 (x:xs) = (sumllR' x) + sumllR'2  xs


-- sumllP :: Num a => [[a]] -> a
sumllP xs = foldr (\x y ->  y + sum x ) 0  xs 


borraR :: Eq a => a -> [a] -> [a]
borraR n [] = []
borraR n (x:xs)
    | x == n = borraR n xs 
    | otherwise = x:  borraR n xs

borraP n xs = foldr (\ x y -> if x == n then y else x:y ) [] xs


diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR ys xs =    
    diferenceItem (iterarItem ys xs) ys
    where 
        includeItem b ax  = [a | a <- ax, b == a ]
        iterarItem [] _ = []
        iterarItem (c:cx) dx = includeItem c dx  ++  iterarItem cx dx

        diferenceItem [] a = a
        diferenceItem (z:zx) px = diferenceItem  zx (borraR z px)


-- diferenciaP :: Eq a => [a] -> [a] -> [a]
diferenciaP (y:ys) xs = foldl (\a b -> if a == [b] then [b] else a) [0] xs
diferenciaP xs ys = foldl (flip borraR) xs ys   


producto :: Num a => [a] -> a
producto xs = foldl (\x y -> abs(x) * abs(y)) 1 xs 

-- productoPred :: Num a => (a -> Bool) -> [a] -> a
productoPred f xs = foldl (\x y -> if f y then x*y else x) 1 xs

productoPos xs = foldl (\x y -> if  y > 0 then x*y else x) 1 xs


productoPos' xs = productoPred (>0) xs

colas [] = []
colas (x:xs) = tail (x:xs) : colas xs

colas' [] = [[]]
colas' (x:xs) = take (1+length (x:xs) - 1) (x:xs) : colas' xs

colas'' :: [a] -> [[a]]
colas'' [] = [[]]
colas'' (x:xs) = (x:xs) : colas'' xs


--cabezas :: [a] -> [[a]]

-- cabezas [a] = [[a]]
cabezas (y:ys) = 
    cabezas'  (reverse (y:ys))
    where 
        cabezas' [] = [[]]
        cabezas' (x:xs) =  cabezas'  xs ++ [take (length xs + 1) (y:ys)]

cabezas2 :: [a] -> [[a]]
cabezas2 [] = [[]]
cabezas2 (x:xs) = [] : [x:ys | ys <- cabezas2 xs]


cabezasP :: [a] -> [[a]]
cabezasP xs = foldl (\x y -> x ++ [ take (length x) xs   ] ) [[]] xs

cabezasP2 :: [a] -> [[a]]
cabezasP2 = foldr (\x y -> [x]:[x:ys | ys <- y]) []