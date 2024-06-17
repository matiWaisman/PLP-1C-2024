-- Ejercicio 3
sumFoldr :: Num a => [a] -> a
sumFoldr = foldr (+) 0

elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr e = foldr (\x rec -> x == e || rec) False

concatFoldr :: [a] -> [a] -> [a]
concatFoldr l1 l2 = foldr (\x rec ->  x : rec) l2 l1

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (\x rec -> if f x then x : rec else rec) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\acc x -> if null acc then [x] else acc ++ [last acc + x]) []

-- Ejercicio 6
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if e == x then xs else x : rec) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n = recr (\x xs rec -> if n > x then x : rec else n : x : xs) [n]

-- Ejercicio 11
foldNat :: (Int -> a -> a) -> a -> Int -> a
foldNat f baseCase 0 = baseCase
foldNat f baseCase n = f n (foldNat f baseCase (n - 1))

potencia :: Int -> Int -> Int
potencia base exponente = foldNat (\_ rec -> rec * base) 1 exponente

-- Ejercicio 12
data Polinomio a = X -- Caso base
                | Cte a -- Caso base
                | Suma (Polinomio a) (Polinomio a) -- Caso recursivo
                | Prod (Polinomio a) (Polinomio a) -- Caso recursivo

foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli x fCte fSuma fProd polinomio = case polinomio of
    X -> x
    Cte a -> fCte a
    Suma p q -> fSuma (rec p) (rec q)
    Prod p q -> fProd (rec p) (rec q)
    where rec poli = foldPoli x fCte fSuma fProd poli

evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPoli n id (+) (*)

-- Ejercicio 13
data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil fArbol Nil = cNil
foldAB cNil fArbol (Bin i v d) = fArbol (rec i) v (rec d)
    where rec = foldAB cNil fArbol

recAB :: b -> (AB a -> b -> a -> AB a -> b -> b) -> AB a -> b
recAB cNil fArbol Nil = cNil
recAB cNil fArbol (Bin i v d) = fArbol i (rec i) v d (rec d)
    where rec = recAB cNil fArbol

esNil :: AB a -> Bool
esNil Nil = True
esNil (Bin i v d) = False

altura :: AB a -> Int
altura = foldAB 0 (\recI v recD -> 1 + max recI recD)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\recI v recD -> 1 + recI + recD)

mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f (Bin i v d) = foldAB (v) (\recI v recD -> if f recI v && f recD v then v else (if f v recI && f recD recI then recI else recD)) (Bin i v d)

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\sI recI v sD recD -> (recI && recD) && evaluar sI v sD)
    where
        evaluar sI v sD = case (esNil sI, esNil sD) of
            (True, True) -> True
            (False, True) -> raiz sI < v
            (True, False) -> raiz sD > v
            (False, False) -> raiz sI < v && raiz sD > v
        raiz (Bin _ v _) = v

-- Ejercicio 14 
ramas :: AB a -> [[a]]
ramas = foldAB [[]] combinar
    where
        combinar rI v rD = case (null rI, null rD) of
            (True, True) -> [[v]]
            (_, _) -> map (v:) rI ++ map (v:) rD

cantHojas :: AB a -> Int
cantHojas = foldAB 0 (\recI v recD -> if recI == 0 && recD == 0 then 1 else recI + recD)

espejo :: AB a -> AB a
espejo = foldAB Nil (\recI v recD -> Bin recD v recI)

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura a b = recAB True (\sI rI v sD rD -> rI && rD && (mismaEstructuraEnAlgunLado (Bin sI v sD) b)) a

mismaEstructuraEnAlgunLado :: AB a -> AB b -> Bool
mismaEstructuraEnAlgunLado a = recAB False (\sI rI v sD rD -> rI || rD || altura sI + altura sD + 1 == altura a)

truncar :: AB a -> Int -> AB a
truncar Nil _ = Nil
truncar (Bin i r d) n = if n == 0 then Nil
    else Bin (truncar i (n-1)) r (truncar d (n-1))

-- Ejercicio 15
data AIH a = Hoja a | BinAIH (AIH a) (AIH a)

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH fBase _ (Hoja a) = fBase a
foldAIH fBase fRecu (BinAIH a b) = fRecu (foldAIH fBase fRecu a) (foldAIH fBase fRecu b)

sizeAih :: AIH a -> Int
sizeAih = foldAIH (const 0) (\rI rD -> if rI == 0 && rD == 0 then 1 else rI + rD)

-- Ejercicio 16 
data RoseTree a = Rose a [RoseTree a]

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose x xs) = f x (map (foldRose f) xs)

hojas :: RoseTree a -> [a]
hojas = foldRose (\x xs -> if length xs == 0 then [x] else concat xs)

distancias :: RoseTree a -> [Int]
distancias = foldRose (\x xs -> if length xs == 0 then [0] else concatMap (map (+1)) xs)

alturaRt :: RoseTree a -> Int
alturaRt = foldRose (\x xs -> if length xs == 0 then 1 else 1 + maximo xs)

maximo :: [Int] -> Int
maximo = foldr (\x xs -> max x xs) 0

exampleRoseTree :: RoseTree Int
exampleRoseTree = Rose 0 [Rose 1 [Rose 2 []], Rose 3 [Rose 4 [], Rose 5 [Rose 6 []]]]

-- Recu 1C2023
data Componente = Contenedor | Motor | Escudo | Cañón deriving Eq
data NaveEspacial = Modulo Componente NaveEspacial NaveEspacial | Base Componente deriving Eq

recNave :: (Componente -> NaveEspacial -> NaveEspacial -> b -> b -> b) -> (Componente -> b) -> NaveEspacial -> b
recNave _ fBase (Base c) = fBase c
recNave fModulo fBase (Modulo c n1 n2) = fModulo c n1 n2 (rec n1) (rec n2)
    where rec = recNave fModulo fBase

foldNave :: (Componente -> b -> b -> b) -> (Componente -> b) -> NaveEspacial -> b
foldNave fNave fBase = recNave (\c _ _ r1 r2 -> fNave c r1 r2) fBase

espejoNave :: NaveEspacial -> NaveEspacial
espejoNave = foldNave (\c r1 r2 -> Modulo c r2 r1) Base

esSubnavePropia :: NaveEspacial -> NaveEspacial -> Bool
esSubnavePropia n1 n2 = recNave (\c nI nD rI rD -> rI || rD || nI == n2 || nD == n2) (const False) n1

truncarNave :: NaveEspacial -> Integer -> NaveEspacial
truncarNave = foldNave (\c rI rD -> \i -> if i == 0 then (Base c) else (Modulo c (rI (i-1)) (rD (i-1)))) (\ c -> \ i -> Base c )

-- Parcial 1C2023
data HashSet a = Hash (a -> Integer) (Integer -> [a])
vacio :: (a -> Integer) -> HashSet a
vacio f = Hash f (\_ -> [])

pertenece :: Eq a => a -> HashSet a -> Bool
pertenece e (Hash fMap fRet) = elem e (fRet (fMap e))

agregar :: Eq a => a -> HashSet a -> HashSet a
agregar e (Hash fMap fRet) = if pertenece e (Hash fMap fRet) then Hash fMap fRet else Hash fMap (\x -> if fMap e == x then fRet (fMap e) ++ [e] else fRet x)

interseccion :: Eq a => HashSet a -> HashSet a -> HashSet a
interseccion (Hash fMap1 fRet1) (Hash fMap2 fRet2) = Hash fMap1 (\x -> interseccionListas (fRet1 x) (fRet2 x))

interseccionListas :: Eq a => [a] -> [a] -> [a]
interseccionListas l1 l2 = foldr(\x rec -> if elem x l2 then x : rec else rec) [] l1

foldr11 :: (a -> a -> a) -> [a] -> a
foldr11 f l = if length l == 1 then (error "Lista vacia") else recr (\x xs -> f x) (last l) l

-- Recu 2do Cuatri 2022
data Matriz a = NuevaMatriz a | Agregar a Int Int (Matriz a)
foldMatriz :: (a -> b) -> (a -> Int -> Int -> b -> b) -> Matriz a -> b
foldMatriz fNuevaMatriz _ (NuevaMatriz m) = fNuevaMatriz m
foldMatriz fNuevaMatriz fAgregar (Agregar a x y b) = fAgregar a x y (rec b)
    where rec = foldMatriz fNuevaMatriz fAgregar 
ver :: Int -> Int -> Matriz a -> a 
ver x y = foldMatriz id (\e f c rec -> if f == x && y == c then e else rec)

sumarATodos :: Num a => a -> Matriz a -> Matriz a 
sumarATodos n = foldMatriz (\b -> NuevaMatriz (b + n)) (\e f c -> Agregar (e + n) f c)

sumaMatrices :: Num a => Matriz a -> Matriz a -> Matriz a 
sumaMatrices m1 m2 = foldMatriz (\base1 -> sumarATodos base1 m2) (\e f c -> Agregar (e + ver f c m2) f c) m1

miReverse :: [a] -> [a]
miReverse = foldl (flip(:)) []

ponerAlFinal :: a -> [a] -> [a]
ponerAlFinal x = foldr (:) (x:[])

main :: IO ()
main = do
    print (ponerAlFinal 1 [2,3,4])