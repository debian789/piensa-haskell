-- 2.1. Suma de los cuadrados de los n primeros números
sumaDeCuadrados :: Integer -> Integer
sumaDeCuadrados xs = sum (map (^2) [1..xs])
-- R: sumaDeCuadrados 4                        30


-- 2.1. Suma de los cuadrados de los n primeros números
sumaDeCuadrados' :: Integer -> Integer
sumaDeCuadrados' n = sum [x^2 | x <- [1..n]]
-- R: sumaDeCuadrados' 4                       30


--2.2 List con un elemento replicado 
replica :: Int -> a -> [a]
replica n x = [ x | _ <- [1..n]]
-- R: replica 5 2                              [2,2,2,2,2]


-- 2.3. Triángulos aritméticos
suma :: (Num a, Enum a) => a -> a
suma a = sum [x | x <- [1..a]]
-- R: suma 5                                   15


-- 2.3. Triángulos aritméticos
suma' :: (Num a, Enum a) => a -> a
suma' a = sum [1..a]
-- R: suma' 5                                  15


-- Ejercicio 2.3.2. Los triángulo aritmético se forman como sigue
linea :: (Eq a, Num a, Enum a) => a -> [a]
linea 1 = [1] 
linea n = 
    position
    where 
        initial = n - 2
        initialSuma = sum [1..initial]
        positionInitial  = n + initialSuma
        positionFinal  = (n - 1) + positionInitial
        position = [positionInitial..positionFinal]
-- R: linea 5                                  [11,12,13,14,15]


-- Ejercicio 2.3.2. Los triángulo aritmético se forman como sigue
linea' :: (Num a, Enum a) => a -> [a]
linea' n = [suma (n-1) +1 .. suma n]
-- R: linea' 5                                 [11,12,13,14,15]


-- Ejercicio 2.3.3. Definir la función triangulo tal que (triangulo n) es el triángulo aritmético de altura n
triangulo :: (Num a, Enum a, Eq a) => a -> [[a]]
triangulo n =  [ linea x | x <- [1..n]]
-- triangulo 3                                 [[1],[2,3],[4,5,6]]


-- 2.4 Numeros perfectos
perfectos :: Int -> [Int]
perfectos n = 
    resutlado
    where 
        numeroPrimos  = [x | x <- [1..n], primo x == True ]
        operation = map (\n -> 2 ^ (n-1) * ((2 ^ n) - 1)) numeroPrimos
        resutlado = [x |x <- operation, (x < n && x > 0)]

primo :: Integral a => a -> Bool
primo n =
    esPrimo
    where 
        factores = [x | x <- [1..n], n `mod` x == 0]
        esPrimo = factores == [1,n]
-- R: perfectos 855                            [6,28,496] 


-- 2.4. Números perfectos
perfectos' :: Integral a => a -> [a]
perfectos' n = [x | x <- [1..n], sum (init ( factores  x)) == x]
-- R: perfectos' 855                           [6,28,496]


factores :: Integral a => a -> [a]
factores n = [x | x <- [1..n], n `mod` x == 0]
-- R:  factores 21                             [1,3,7,21] 


-- 2.5 Numeros abundantes
numeroAbundante :: Integral a => a -> Bool
numeroAbundante n = 
    abundancia
    where 
        divisibles = [ x | x <-  [1..n], mod n x == 0]
        sumatoria  = sum (divisibles) - 2 * (n)
        abundancia = sumatoria > 0
-- R: numeroAbundante 12                       True

-- 2.5. Números abundantes
numeroAbundante' :: Integral a => a -> Bool
numeroAbundante' n = n < sum (divisores n)

divisores :: Integral a => a -> [a]
divisores n = [m | m <- [1..n-1], n `mod` m == 0]
-- R: numeroAbundante' 18                      True


-- Ejercicio 2.5.2. Definir la función numerosAbundantesMenores t
numerosAbundantesMenores :: Integral a => a -> [a]
numerosAbundantesMenores n = [x | x <- [1..n], numeroAbundante x]
-- R: numerosAbundantesMenores 50              [12,18,20,24,30,36,40,42,48]


-- 2.6 Problema 1 del proyecto Euler
euler1 :: Integer -> Integer 
euler1 n =
    sum [x | x <- [1..n-1], multiplo x 3 || multiplo x 5]
    where 
        multiplo x y = mod x y == 0
-- R: euler1 50                                543


-- 2.7. Número de pares de naturales en un círculo
circulo :: Int -> Int 
circulo n = length [(x,y) | x <- [0..n], y <- [0..n], x^2+y^2 < n^2]
-- R: circulo 10                               86 


-- 2.8 Aproximacion del numero e
aproxE :: (Enum a, Floating a) => a -> [a]
aproxE n = 
    divicion2
    where
        divicion2 = [ ((m+1) / m) ** m | m <- [1..n]   ]
-- R:  aproxE 3                                 [2.0,2.25,2.37037037037037]


-- 2.9 Aproximacion del limite
aproxLimSeno :: (Enum a, Floating a) => a -> [a]
aproxLimSeno n = 
    suesion     
    where 
        seno a = sin(1/a) / (1/a)
        suesion = [ seno x | x <- [1..n]]
-- R: aproxLimSeno 2                            [0.8414709848078965,0.958851077208406]


-- 2.10. Cálculo del número π
calcularPi :: (Enum a, Floating a) => a -> a
calcularPi n = 
    valor
    where 
        ecuacionPi  x = (-1)**x / ((2*x) + 1)
        valorPiList = [ ecuacionPi x | x <- [0..n]]
        valor = sum (valorPiList) * 4
-- R: calcularPi 100                             3.1514934010709914 


-- 2.11. Ternas pitagóricas
pitagoricas :: Int -> [(Int, Int, Int)]
pitagoricas n = [(x,y,z)| x <- [1..n],
                          y <- [1..n],
                          z <- [1..n],
                          x^2 +y^2 == z^2 ]
-- R: pitagoricas 10                              [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]


-- Ejercicio 2.11.2. Definir la función
numeroDePares :: (Integral a1, Num a2) => (a1, a1, a1) -> a2
numeroDePares (a, b ,c) = sum [1 | n <- [a,b,c], even n]
-- R: numeroDePares (68,88,84)                     3 


-- 2.13 Producto escalar
productoEscalar :: Num a => [a] -> [a] -> a
productoEscalar xs xy =  
    mult
    where 
        mult = sum [x*y|  (x,y) <- zip xs xy ]
-- R: productoEscalar [1,2,3] [4,5,6]              32


-- 2.14 Suma de pares de elementos consecutivos 
sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos [] = []
sumaConsecutivos [a] = []
sumaConsecutivos (x:xs) = [x +  xs !! 0] ++ sumaConsecutivos xs
-- R: sumaConsecutivos [1,2,3,4,5]                    [3,5,7,9] 


-- 2.14. Suma de pares de elementos consecutivosx
sumaConsecutivos' :: [Int] -> [Int]
sumaConsecutivos' xs = [x+y | (x,y) <- zip xs (tail xs)] 
-- R: sumaConsecutivos' [1,2,3,4,5]                   [3,5,7,9]

-- 2.15. Posiciones de un elemento en una lista
posiciones :: Eq a => a -> [a] -> [a]
posiciones a [] = []
posiciones a (x:xs) 
    | x /= a    = [x] ++ posiciones a xs  
    | otherwise = posiciones a xs
-- R: posiciones 2 [1,2,3,4,5]                        [1,3,4,5]


-- 2.15. Posiciones de un elemento en una lista
posiciones' :: Eq a => a -> [a] -> [Int]
posiciones' x xs = 
    [i | (x', i) <- zip xs [0..n], x == x']
    where n = length xs - 1
-- R: posiciones' 3 [1,2,3,4,5]                       [2]


-- 2.17 Producto cartesiano
pares :: [a] -> [b] -> [(a, b)]
pares xy xz = [(x,y) | x <- xy, y <- xz]
-- R: pares [1,2] [4,5]                                [(1,4),(1,5),(2,4),(2,5)] 


-- 2.18 Consulta de base de datos 
personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
            ("Velazquez","Pintura",1599,1660),
            ("Picasso","Pintura",1881,1973),
            ("Beethoven","Musica",1770,1823),
            ("Poincare","Ciencia",1854,1912),
            ("Quevedo","Literatura",1580,1654),
            ("Goya","Pintura",1746,1828),
            ("Einstein","Ciencia",1879,1955),
            ("Mozart","Musica",1756,1791),
            ("Botticelli","Pintura",1445,1510),
            ("Borromini","Arquitectura",1599,1667),
            ("Bach","Musica",1685,1750)]


-- Ejercicio 2.18.1. Definir la función nombres tal que (nombres bd) es la lista de los nombres
nombres :: [(a, b, c, d)] -> [a]
nombres xs = [n | (n,_,_,_) <- xs]
-- R: nombres personas               ["Cervantes","Velazquez","Picasso","Beethoven","Poincare","Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini","Bach"] 


-- Ejercicio 2.18.2. Definir la función musicos tal que (musicos bd)
musicos :: [(a, [Char], c, d)] -> [a]
musicos xs = [n | (n,m,_,_) <- xs, m == "Musica" ]
-- R: musicos personas                ["Beethoven","Mozart","Bach"] 


-- Ejercicio 2.18.3. Definir la función seleccion tal que (seleccion bd m)
seleccion :: Eq a1 => [(a2, a1, c, d)] -> a1 -> [a2]
seleccion xs x = [a | (a,m,_,_) <- xs, m == x]
-- R: seleccion  personas "Pintura"   ["Velazquez","Picasso","Goya","Botticelli"] 


-- Ejercicio 2.18.4. Definir, usando el apartado anterior, la función musicos'
musicos' :: [(a2, [Char], c, d)] -> [a2]
musicos' xs = seleccion xs "Musica"
--R: musicos'   personas              ["Beethoven","Mozart","Bach"] 


-- Ejercicio 2.18.5. Definir la función vivas tal que (vivas bd a)
vivas :: Ord b1 => [(a, b2, b1, b1)] -> b1 -> [(a, b1)]
vivas xs y = [(a,yy) | (a,_,yy,zz) <- xs, y > yy && zz > y]
-- R: vivas   personas 1880               [("Poincare",1854),("Einstein",1879)] 

-- Ejercicio 2.18.5. Definir la función vivas tal que (vivas bd a)
vivas' :: Ord b1 => [(a, b2, b1, b1)] -> b1 -> [(a, b1)]
vivas' ps a = [(x,a1) | (x,_,a1,a2) <- ps, a1 <= a, a <= a2]
-- R: vivas'   personas 1880           [("Poincare",1854),("Einstein",1879)]