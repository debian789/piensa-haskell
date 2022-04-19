import Data.Char
import Data.List
sumaDigitosC :: String -> Int
sumaDigitosC xs = sum[digitToInt x | x <- xs, isDigit x  ]


sumaDigitosR :: String -> Int
sumaDigitosR [] = 0
sumaDigitosR (x:xs) 
    | isDigit x = digitToInt x + sumaDigitosR xs
    | otherwise = sumaDigitosR xs 

mayusculaInicial :: String -> String
mayusculaInicial [] = []
mayusculaInicial (a:xs) = toUpper (a) : [ toLower x |x <- xs]


mayusculaInicialR :: String -> String
mayusculaInicialR [] = []
mayusculaInicialR (x:xs) = toUpper x : aux xs
    where 
        aux (x:xs) = toLower x : aux xs
        aux []     = []

titulo :: [String] -> [String]
titulo [] = []
titulo (b:bs) = 
    [toUpper (head b) : [(b !! 1)] ] ++ titulo' bs
    where 
        auxLower (a:as) = toLower a : auxLower as 
        auxLower [] = []
        titulo' [] = []
        titulo' (x:xs)
            | length x >= 4 = [(toUpper (x !! 0): tail (auxLower x))]++ titulo' xs
            | length x <  4 = [(auxLower x)] ++ titulo' xs


tituloC :: [String] -> [String]
tituloC (a:ax) =
    capitalice a : [validationConvert aa | aa <- ax ]
    where         
        convertToUpper ys = [toLower y | y <- ys]
        capitalice (y:ys) = toUpper y : convertToUpper (ys)
        validationConvert xs 
            | length xs >= 4 = capitalice xs 
            | length xs <  4 = convertToUpper xs 

buscaCrucigramaC :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaC letter position size xs =
    [x | x <- xs, 
        buscarPalabra letter x, 
        validatePosition letter position x, 
        validateSize size x]
    where 
        buscarPalabra letter' xs' =  elem letter' xs'
        validatePosition letter' position' xs' 
            | letter' == xs' !! position' = True 
            | otherwise = False
        validateSize size' xs' 
            | length xs' == size' = True 
            | otherwise = False

buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama l pos lon ps =
    [p | p <- ps,
        length p == lon,
        0 <= pos, pos < length p,
        p !! pos == l]


buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigramaR _      _        _    [] =  []
buscaCrucigramaR letter position size (x:xs) 
    | validatePosition letter position x &&  validateSize size x  = x : buscaCrucigramaR letter position size xs
    | otherwise =  buscaCrucigramaR letter position size xs
    where 
        validatePosition letter' position' xs' 
            | letter' == xs' !! position' = True 
            | otherwise = False
        validateSize size' xs' 
            | length xs' == size' = True 
            | otherwise = False

posiciones :: String -> Char -> [Int]
posiciones xs letter = [yy |  (xx,yy) <- zip xs [0..], xx == letter ]

posicionesR :: String -> Char -> [Int]
posicionesR xs letter = 
    iteracion xs 0 letter
    where 
        iteracion [] _ _ = []
        iteracion (x:xy) n letter' 
            | letter' == x =  n : iteracion xy (n + 1) letter'
            | otherwise = iteracion xy (n + 1) letter'

contieneR :: String -> String -> Bool
contieneR [] [] = True
contieneR  [] _ = False
contieneR (x:xs) ys 
    | isPrefixOf ys (x:xs) = True
    | otherwise = contieneR xs ys

contieneR' :: String -> String -> Bool
contieneR' _ [] = True
contieneR' [] ys = False
contieneR' xs ys = isPrefixOf ys xs || contieneR' (tail xs) ys



unirPalabras xs = [x | x <- xs, x /= ' ']

unirPalabras' :: [[a]] -> [a]
unirPalabras' = concat

conteoPalabras xs = [length x | x <- xs]

conteoPalabras' :: [[a]] -> [Int]
conteoPalabras' = map length

agrupar4 [] = []
agrupar4 (x:xs) = 
    tomar (removeSpace (x:xs)) :  agrupar4 (limpiar (removeSpace (x:xs)))
    -- agrupar4 ( drop 3 (x:xs))
    where
        removeSpace (x:xs) = unirPalabras (x:xs)
        tomar (x:xs)   = take 4 (x:xs)
        limpiar (x:xs) = drop 4 (x:xs) 
        

agrupar4' :: [a] -> [[a]]
agrupar4' [] = []
agrupar4' xs = take 4 xs : agrupar4' (drop 4 xs)



seprarPalabras' (x:xs) = [(a,b) | (a,b) <- zip (seprarPalabras (x:xs)) [0..] ]

seprarPalabras []  = [[]]
seprarPalabras (x:xs) 
    | x == ' ' = ['-'] : seprarPalabras xs
    | x /= ' ' = [x] : seprarPalabras xs
    | otherwise = seprarPalabras xs


divide :: (a -> Bool) -> [a] -> ([a], [a])
divide a xs = (takeWhile a xs, dropWhile a xs)

divide' :: (a -> Bool) -> [a] -> ([a], [a])
divide' = span

palabras' :: String -> [String]
palabras' xs = words xs

palabras :: String -> [String]
palabras [] = []
palabras cs = cs1 : palabras cs2
    where 
        cs' = dropWhile (==' ') cs
        (cs1,cs2) = divide (/=' ') cs'


inversas :: [[a]] -> [[a]]
inversas xs = [reverse x | x <- xs]

inversas' :: [[a]] -> [[a]]
inversas' = map reverse 

-- agrupa :: [a] -> [Int] -> [[a]]
-- agrupa xs ns = map (\x -> take x xs) ns 
agrupa "" [] = []
agrupa xs (n:ns) = take n xs : agrupa ( drop n xs) ns


frase :: [String] -> String
frase [a] = a
frase (x:xs) = x ++ " " ++  (frase xs)


frase' :: [String] -> String
frase' = unwords



ceros :: Int -> Int
ceros ns = 
    zero (show ns)
    where 
        zero ['0'] = 1
        zero [] = 0
        zero (n:ns)    
            | last (n:ns) == '0' = 1 + zero (init ns)
            | otherwise = zero (ns)


ceros' :: Int -> Int
ceros' n = length (takeWhile (=='0') (reverse (show n)))