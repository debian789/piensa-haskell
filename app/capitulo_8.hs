import Data.Char
import Data.List



type Punto = (Double, Double)

data Nat = Cero | Suc Nat 
            deriving (Eq, Show)



suma :: Nat -> Nat -> Nat 
suma Cero n    = n 
suma (Suc m) n = Suc (suma m n )
-- suma (Suc (Suc Cero)) (Suc (Suc (Suc Cero)))                   Suc (Suc (Suc (Suc (Suc Cero))))


producto :: Nat -> Nat -> Nat
producto Cero n    = n 
producto (Suc m) n = suma m (producto m n)
-- producto (Suc (Suc Cero)) (Suc (Suc (Suc Cero)))                 Suc (Suc (Suc (Suc Cero)))


data Arbol = Hoja Int | Nodo Arbol Int Arbol 
            deriving (Show, Eq)

ejArbol :: Arbol
ejArbol = Nodo (Nodo (Hoja 1) 3 (Hoja 4)) 5  (Nodo (Hoja 6) 7 (Hoja 9))

data Ordering = LT | EQ | GT


data Palo = Picas | Corazones | Diamantes | Treboles
            deriving (Eq, Show)

data Color = Negro | Rojo 
            deriving (Show)


color :: Palo -> Color
color p 
    | p == Picas || p == Treboles = Negro 
    | p == Corazones || p == Diamantes = Rojo 

color' :: Palo -> Color
color' Picas     = Negro 
color' Corazones = Rojo
color' Diamantes = Rojo 
color' Treboles  = Negro 


data Valor = Sota | Rey | Reina | As | Numerico Int
                deriving (Eq, Show)

mayor :: Valor -> Valor -> Bool
mayor _ As = False
mayor As _ = True
mayor _ Rey = False
mayor Rey _ = True
mayor _ Reina = False
mayor Reina _ = True
mayor _ Sota = False
mayor Sota _ = True
mayor (Numerico m) (Numerico n) = m > n


data Carta = Carta Valor Palo
            deriving (Eq, Show)

valor :: Carta -> Valor
valor (Carta v p) = v 
-- valor (Carta Rey Corazones)          Rey



palo :: Carta -> Palo
palo (Carta v p) = p



ganaCarta :: Palo -> Carta -> Carta -> Bool
ganaCarta p (Carta v1 p1) (Carta v2 p2) 
    | mayor v1  v2 = True 
    | otherwise = False

-- ganaCarta Corazones (Carta Sota Picas) (Carta (Numerico 5) Picas)    True



data Mano = Vacio | Agregar  Carta  Mano 
    deriving (Eq, Show)



data Expr  = Num Int | Suma Expr Expr | X 
            deriving (Eq, Show)


numVars :: Expr -> Int
numVars Num c = 0
numVars Suma (a b) = 

    | Suma (a b) = 
    
    
    Suma X (Suma (Num 13) X)

