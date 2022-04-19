-- 2.1 Suma de los cuadrados de los n primeros numeros 
sumaDeCuadrados :: Integer -> Integer
sumaDeCuadrados xs = sum (map (^2) [1..xs])

sumaDeCuadrados' :: Integer -> Integer
sumaDeCuadrados' n = sum [x^2 | x <- [1..n]]

--2.2 List con un elemento replicado 
replica :: Int -> a -> [a]
replica n x = [ x | _ <- [1..n]]


-- 2.3 Triangulos aritmeticos
suma a = sum [x | x <- [1..a]]
suma' a = sum [1..a]

linea 1 = [1] 
linea n = 
    position
    where 
        initial = n - 2
        initialSuma = sum [1..initial]
        positionInitial  = n + initialSuma
        positionFinal  = (n - 1) + positionInitial
        position = [positionInitial..positionFinal]

linea' n = [suma (n-1) +1 .. suma n]

triangulo n =  [ linea x | x <- [1..n]]

-- 2.4 Numeros perfectos
perfectos :: Int -> [Int]
perfectos n = 
    resutlado
    where 
        numeroPrimos  = [x | x <- [1..n], primo x == True ]
        operation = map (\n -> 2 ^ (n-1) * ((2 ^ n) - 1)) numeroPrimos
        resutlado = [x |x <- operation, (x < n && x > 0)]
        -- operation = [x | x <- numeroPrimos ]

primo n =
    esPrimo
    where 
        factores = [x | x <- [1..n], n `mod` x == 0]
        esPrimo = factores == [1,n]



perfectos' n = [x | x <- [1..n], sum (init ( factores  x)) == x]
factores n = [x | x <- [1..n], n `mod` x == 0]

-- 2.5 Numeros abundantes
numeroAbundante n = 
    abundancia
    where 
        divisibles = [ x | x <-  [1..n], mod n x == 0]
        sumatoria  = sum (divisibles) - 2 * (n)
        abundancia = sumatoria > 0
        
        
numeroAbundante' n = n < sum (divisores n)

divisores n = [m | m <- [1..n-1], n `mod` m == 0]


numerosAbundantesMenores n = [x | x <- [1..n], numeroAbundante x]

-- 2.6 Problema 1 del proyecto Euler
euler1 :: Integer -> Integer 
euler1 n =
    sum [x | x <- [1..n-1], multiplo x 3 || multiplo x 5]
    where 
        multiplo x y = mod x y == 0


circulo :: Int -> Int 
circulo n = length [(x,y) | x <- [0..n], y <- [0..n], x^2+y^2 < n^2]

-- 2.8 Aproximacion del numero e
aproxE n = 
    divicion2
    where
        divicion2 = [ ((m+1) / m) ** m | m <- [1..n]   ]


-- 2.9 Aproximacion del limite
aproxLimSeno n = 
    suesion 
    
    where 
        seno a = sin(1/a) / (1/a)
        suesion = [ seno x | x <- [1..n]]


-- 2.10 Calculo del numero pi
calcularPi n = 
    valor
    where 
        ecuacionPi  x = (-1)**x / ((2*x) + 1)
        valorPiList = [ ecuacionPi x | x <- [0..n]]
        valor = sum (valorPiList) * 4

-- 2.11 Ternas pitagoricas
pitagoricas :: Int -> [(Int, Int, Int)]
pitagoricas n = [(x,y,z)| x <- [1..n],
                          y <- [1..n],
                          z <- [1..n],
                          x^2 +y^2 == z^2 ]

numeroDePares (a, b ,c) = sum [1 | n <- [a,b,c], even n]

-- 2.13 Producto escalar
-- productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs xy =  
    mult
    where 
        mult = sum [x*y|  (x,y) <- zip xs xy ]


productoEscalar' (a,b,c) (d,e,f) = sum [a*d, b*e, c*f]

-- 2.14 Suma de pares de elementos consecutivos 
sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos [] = []
sumaConsecutivos [a] = []
sumaConsecutivos (x:xs) = [x +  xs !! 0] ++ sumaConsecutivos xs

sumaConsecutivos' :: [Int] -> [Int]
sumaConsecutivos' xs = [x+y | (x,y) <- zip xs (tail xs)] 


-- posiciones :: Eq a => a -> [a] -> [Int]
posiciones a [] = []
posiciones a (x:xs) 
    | x /= a    = [x] ++ posiciones a xs  
    | otherwise = posiciones a xs



posiciones' :: Eq a => a -> [a] -> [Int]
posiciones' x xs = 
    [i | (x', i) <- zip xs [0..n], x == x']
    where n = length xs - 1

-- 2.17 Producto cartesiano
pares xy xz = [(x,y) | x <- xy, y <- xz]

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


nombres xs = [n | (n,_,_,_) <- xs]

musicos xs = [n | (n,m,_,_) <- xs, m == "Musica" ]

seleccion xs x = [a | (a,m,_,_) <- xs, m == x]

musicos' xs = seleccion xs "Musica"

vivas xs y = [(a,yy) | (a,_,yy,zz) <- xs, y > yy && zz > y]

vivas' ps a = [(x,a1) | (x,_,a1,a2) <- ps, a1 <= a, a <= a2]