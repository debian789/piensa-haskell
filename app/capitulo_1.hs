media3 :: Fractional a => a -> a -> a -> a
media3 a b c = do 
    (a + b + c) / 3 
-- R: media3 2 3 4                       3.0


sumaMonedas :: Num a => a -> a -> a -> a -> a -> a
sumaMonedas a b c d e  = 
    aa + bb +cc +dd +ee
    where 
        aa = a * 1
        bb = b * 2
        cc = c * 5
        dd = d * 10
        ee = e * 20
-- R: sumaMonedas 1 2 3 4 5              160

volumenEsfera2 :: Floating a => a -> a
volumenEsfera2 a = (4/3) * pi * (a^3)
-- R: volumenEsfera2 10                  4188.790204786391

areaDeCoronaCircular :: Floating a => a -> a -> a
areaDeCoronaCircular r1 r2 = pi * (r2 ^ 2  - r1 ^ 2)
-- R: areaDeCoronaCircular 10 100         31101.767270538952

ultimaCifra :: Show a => a -> Char
ultimaCifra a = last (show(a))
-- R:  ultimaCifra 789                    '9'

ultimaCifra' :: Integral a => a -> a
ultimaCifra' a = rem a 10
-- ultimaCifra' 789                        9

maxTres :: Ord a => a -> a -> a -> a
maxTres a b c 
    | a > b && a > c = a
    | b > c && b > a = b
    | c > b && c > a = c
-- R:  maxTres 10 22 3                     22
    
maxTres' :: Ord a => a -> a -> a -> a
maxTres' a b c = max a (max b c)
-- maxTres' 10 22 3                        22

xor1 :: Eq a => a -> a -> Bool
xor1 a b 
    | a == b = False
    | otherwise = True
-- R: xor1 3 3                             False


rotar1 :: [a] -> [a]
rotar1 [] = []
rotar1 (x:xs) = rotar1 xs ++ [x]
-- R: rotar1 [1,2,3,4]                     [4,3,2,1]


rotar1' :: [a] -> [a]
rotar1' a = reverse a
-- R: rotar1' [1,2,3,4]                    [4,3,2,1]

rotar1'' :: [a] -> [a]
rotar1'' xs =  tail xs ++ [head xs]
-- R: rotar1'' [1,2,3,4]                   [2,3,4,1]


rota :: Int -> [a] -> [a]
rota a xs =   drop a xs ++ take a xs 
-- R: rota 2 [6,7,9,10]                    [9,10,6,7]

rango :: (Foldable t, Ord a) => t a -> [a]
rango xs = [minimum xs, maximum xs]
-- R: rango  [12,8,7,10,11]                [7,12]


-- Reconocimiento de aplindromos 

palindromo :: Eq a => [a] -> Bool
palindromo a 
    | a == reverse a = True
    | otherwise      = False
-- R: palindromo [3,2,3]                    True

palindromo' :: Eq a => [a] -> Bool
palindromo' a = a == reverse a
-- R: palindromo' [3,2,3]                    True


-- 1.11 Elementos interiores de una lista
interior :: [a] -> [a]
interior xs = init(tail xs)
-- R: interior [1,2,3,4,5]                   [2,3,4]

interior' :: [a] -> [a]
interior' xs =  tail(init xs)
-- R: interior' [1,2,3,4,5]                  [2,3,4]

-- 1.12 Finales de una lista
finales :: Int -> [a] -> [a]
finales n xs = drop ((length xs) - n) xs 
-- R: finales 3 [1,2,3,4]                    [2,3,4]

-- 1.13 Segmentos de una lista 
segmentos :: Int -> Int -> [a] -> [a]
segmentos a b xs = drop (a-1) (take b xs) 
-- R: segmentos  2 4 [9,8,7,6,5]              [8,7,6]

-- 1.14 Extremos de una lista 
extremos :: Int -> [a] -> [a]
extremos a xs = take a xs ++ drop (length xs - a) xs
-- R: extremos   6 [1,2,3]                    [1,2,3,1,2,3]

-- 1.15 Mediano 3 numeros
mediano :: (RealFrac a, Integral b) => a -> a -> a -> b
mediano a b c = round ((a + b + c) / 3)
-- R: mediano 3 4 7                              5


-- 1.16 Igualdad y diferencia de 3 elementos 
tresIguales :: Eq a => a -> a -> a -> Bool
tresIguales a b c = (a == b && b == c && c == a)
-- R: tresIguales 6 6 6                          True 

tresDiferentes :: Eq a => a -> a -> a -> Bool
tresDiferentes a b c = (a /= b && b /= c && a /= c )
-- R: tresDiferentes 3 4 5                       True 


-- 1.17 Igual de 4 Elementos
cuatroIguales :: Eq a => a -> a -> a -> a -> Bool
cuatroIguales a b c d = tresIguales a b c && tresIguales b c d 
-- R: cuatroIguales 5 5 5 5                      True

cuatroIguales' :: Eq a => a -> a -> p -> a -> Bool
cuatroIguales' a b c d = a == b && tresIguales a b d
-- R: cuatroIguales' 5 5 5 5                     True


-- Propiedad triangular 
triangular :: (Ord a, Num a) => a -> a -> a -> Bool
triangular a b c = (a + b > c) && (a + c > b) && (c + b > a)
-- R: triangular 3 3 3                           True


-- 1.19 Division segura 
divisionSegura :: (Ord p, Fractional p) => p -> p -> p
divisionSegura a b 
    | b > 0 = a / b
    | otherwise = 9999.0
-- R: divisionSegura  4 0                        9999.0

divisionSegura' :: (Eq p, Fractional p) => p -> p -> p
divisionSegura' _ 0 = 9999.0
divisionSegura' a b = a / b 
-- R: divisionSegura'  4 0                       9999.0

-- 1.21 Modulo de un vector
modulo :: Floating a => (a, a) -> a
modulo (a, b) = sqrt(a^2 + b ^2 )
-- R: modulo (3,4)                                5.0

-- 1.22 Rectangulo de area maxima 
mayorRectangulo :: (Ord b, Num b) => (b, b) -> (b, b) -> (b, b)
mayorRectangulo (a, b) (c,d) 
    | (a * b) >= (c*d) = (a,b)
    | otherwise        = (c,d)
-- R: mayorRectangulo (2,3) (1,8)                 (1,8)


-- 1.23.1 Cuadrante de un punto 
cuadrante :: (Ord a1, Ord a2, Num a1, Num a2, Num p) => (a1, a2) -> p
cuadrante (a,b) 
    |a > 0 && b > 0 = 1
    |a < 0 && b > 0 = 2
    |a < 0 && b < 0 = 3
    |otherwise = 4
-- R: cuadrante (-6,3)                              2


intercambia :: (b, a) -> (a, b)
intercambia (a, b) = (b,a)
-- R: intercambia (10,2)                           (2,10)

-- 1.23.3 Punto simetrico 
simetricoH :: (Ord a1, Num a1) => (a2, a1) -> (a2, a1)
simetricoH (a,b) 
    | b > 0     = (a , negate b)
    | otherwise = (a , -1 * b )
-- R: simetricoH (2,3)                             (2,-3)


simetricoH' :: Num b => (a, b) -> (a, b)
simetricoH' (a,b) = (a,-b)
-- R: simetricoH' (2,3)                            (2,-3)

-- 1.23.4 Distancia entre dos puntos
distancia :: Floating a => (a, a) -> (a, a) -> a
distancia (a1,b1) (a2,b2) = sqrt( (a2 - a1) ^ 2 + (b2 -b1) ^ 2 )
-- R: distancia (2,2) (4,5)                         3.605551275463989

-- punto medio entre otros dos 
puntoMdia :: (Fractional a, Fractional b) => (a, b) -> (a, b) -> (a, b)
puntoMdia (a1,b1) (a2,b2) = ((a2 + a1)/2 , (b2 + b1)/2 )
-- R:  puntoMdia  (2,2) (4,5)                       (3.0,3.5)

-- 1.25 Intercalacion de pares
intercala :: [a] -> [a] -> [a]
intercala [x1,xs1] [x2, xs2] = [x1,xs2,x2,xs1]
-- R: intercala [1,2] [3,4]                         [1,4,3,2]

-- Permutacion ciclica de una lista
ciclo :: [a] -> [a]
ciclo [] = []
ciclo xs = last xs : init xs
-- R: ciclo [1,2,3,4]                               [4,1,2,3]

-- 1.27 Mayor numero de 2 cifras con dos digitos dados 
numeroMayor :: (Show a2, Show a1) => a2 -> a1 -> Int
numeroMayor a b = 
    case numer1 > numer2 of 
        True -> numer1
        False -> numer2
    where 
        aa = show a ++ show b
        bb = show b ++ show a
        numer1 = read aa :: Int
        numer2 = read bb :: Int
-- R: numeroMayor 1 24                               241

numeroMayor' :: (Num a, Ord a) => a -> a -> a
numeroMayor' x y  = a*10 + b
    where a = max x y
          b = min x y  
-- R: numeroMayor' 1 24                              241

-- 1.28 Numero de reaices de una ecuacion cuadratica 
numeroDeRaices :: Int -> Int -> Int -> Int
numeroDeRaices _ 0 _ = 0
numeroDeRaices a b c 
    | d < 0 = 0
    | d == 0 = 1
    | otherwise = 2
    where 
        d = b ^ 2 - (4 * (a*c))
-- R: numeroDeRaices 20 33 4                         2

-- raices de las ecucaciones cuadraticas 
raices :: Floating a => a -> a -> a -> [a]
raices a b c 
    | otherwise  = [positivo , negativo]
    where 
        elevado = b ^2
        cuatro  = (4 * (a*c))
        operacionRaiz = elevado - cuatro
        resultadoRaiz = sqrt(operacionRaiz)
        positivo = (-b + resultadoRaiz ) / (2 * a)
        negativo = (-b - resultadoRaiz ) / (2 * a)
-- R: raices 11 42 33                                   [-1.1062035576065592,-2.711978260575259]


raices' :: Floating a => a -> a -> a -> [a]
raices' a b c = [(-b+d)/t, (-b-d)/t]
    where d = sqrt (b^2 - 4*a*c)
          t = 2 * a
-- R:  raices' 11 42 33                                 [-1.1062035576065592,-2.711978260575259]

-- 1.30 Area de un triangulo mediante la formula de Heron
area :: Fractional a => a -> a -> a -> a
area a b c = (a+b+c)/2
--  area 4 5 6                                           7.5

-- 1.31 Numeros racionales como pares de enteros 
formaReducida :: (Integral a, Integral b) => (a, b) -> (a, b)
formaReducida (a,b) = (a `div` 2, b `div` 2 )
-- R: formaReducida (2,34)                               (1,17)



