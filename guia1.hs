-- Ejercicio 1

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# HLINT ignore "Use record patterns" #-}

max2Curry :: Float -> (Float -> Float)
max2Curry x = \y -> if x > y then x else y

normaVectCurry :: Float -> (Float -> Float)
normaVectCurry x = \y -> sqrt (x^2 + y^2)


-- Ejercicio 2

-- Toma una funcion que es una tupla y elems que no son tupla y devuelve el resultado de aplicarle los elems que no son tupla como tupla
curry' :: ((a,b) -> c) -> a -> (b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y

-- curryN pendiente, como definir tuplas de n elementos en el tipo

-- Ejercicio 3

sumaFoldr :: Num a => [a] -> a
sumaFoldr = foldr (\x acc -> x + acc) 0

elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr e = foldr (\x acc -> x == e || acc) False

concatenarFoldr :: [a] -> [a] -> [a]
concatenarFoldr l1 l2 = foldr (\x acc -> x : acc) l2 l1

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (\x acc -> if f x then x : acc else acc) []

mapFoldr :: (a -> a) -> [a] -> [a]
mapFoldr f = foldr (\x acc -> f x : acc) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x acc -> if f x acc then x else acc)

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\acc x -> if null acc then [x] else acc ++ [last acc + x]) []

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (\x acc -> x - acc) 0

sumaAltInversa :: Num a => [a] -> a
-- sumaAltInversa l = sumaAlt (reverse l)  
--sumaAltInversa = foldl(\acc x -> x - acc) 0
sumaAltInversa l = foldl (flip (-)) 0 l

-- Ejercicio 4

--permutaciones :: [a] -> [[a]]
--permutaciones = 

partes :: [a] -> [[a]]
partes = foldl (\acc x -> acc ++ map (\a -> a ++ [x]) acc) [[]]

prefijos :: [a] -> [[a]]
prefijos = foldl (\acc x  -> acc ++ [last acc ++ [x]]) [[]]

-- sublistas :: [a] -> [[a]]



-- Ejercicio 5

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)

entrelazar2 :: [a] -> [a] -> [a]
entrelazar2 = foldr (\x acc y -> if null y then x : acc y else x : head y : acc (tail y)) id




-- Ejercicio 6
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if x == e then xs else rec) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs rec -> if null xs then [x, e] else if e < x then e: x : xs else x : rec) []

-- Ejercicio 7
genLista :: a -> (a -> a) -> Int -> [a]
genLista elementoInicial f cant = foldr (\x acc -> if null acc then [elementoInicial] else acc ++ [f (last acc)]) [] [1..cant + 1]

desdeHasta :: Int -> Int -> [Int]
desdeHasta d h = genLista d (\x -> x + 1) (h - d)

-- Ejercicio 8
mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f l = foldr (\x acc -> uncurry' f x : acc) [] l

armarPares :: [a] -> [a] -> [(a,a)]
armarPares  = foldr (\x acc y -> if null y then [] else (x, head y) : acc (tail y)) (const [])

mapDoble :: (a -> a -> a) -> [a] -> [a] -> [a]
mapDoble f l1 l2 = mapPares f (armarPares l1 l2)
--mapDoble f = foldr (\x acc y -> f x (head y) : acc (tail y)) (const[])

-- Ejercicio 9

-- Recorro las filas con foldr y las columnas se suman usando mapDoble/ zipWith
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = foldr (\x acc y -> mapDoble (+) x (head y) : acc (tail y)) (const [])

--trasponer :: [[Int]] -> [[Int]]
--trasponer = foldr (\i fila -> fila : foldr (\j columa -> columna : j) []) []

matriz1 :: [[Int]]
matriz1 = [[1, 2, 3], [4, 5, 6]]


-- Ejercicio 10

generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom:: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
                        | otherwise = generateFrom stop next (xs ++ [next xs])


generateBase::Eq a => ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop baseCase f = generate stop (\ls -> if null ls then baseCase else f (last ls))

factorial :: Int -> Int
factorial n = foldr (\x acc-> x * acc) 1 [1..n]

factoriales::Int -> [Int]
factoriales n = generate (\ls -> not (null ls) && (last ls > factorial n)) (\ls -> if null ls then 1 else last ls * (length ls + 1))

iterateN :: Eq a => Int -> (a -> a) -> a -> [a]
iterateN n f baseCase = generateBase (\l -> length l > n) baseCase f

-- Le cambie todos los tipos porque si no como hago para acceder al ultimo elemento de la lista? Para hacer ([a] -> Bool) y ([a] -> a), medio que de eso ya se encarga la funcion iterate y takewhile
generateFrom2:: (a -> Bool) -> (a -> a) -> a -> [a]
generateFrom2 stop next baseCase = takeWhile stop (iterate next baseCase)

-- Ejercicio 11

-- El primer elemento es el fijo y el segundo el que se va reduciendo
foldNat :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
foldNat f x 1 = x
foldNat f x n = f x (foldNat f x (n - 1))

potencia :: Integer -> Integer -> Integer
potencia base exponente = foldNat (*) base exponente

-- Ejercicio 12

data Polinomio a = X
                | Cte a
                | Suma (Polinomio a) (Polinomio a)
                | Prod (Polinomio a) (Polinomio a)

-- Ejercicio 13

data AB a = Nil | Bin (AB a) a (AB a)

foldAb :: b -> (b -> a -> b -> b) -> AB a -> b
foldAb baseCase f Nil = baseCase
foldAb baseCase f (Bin i v d) = f (foldAb baseCase f i) v (foldAb baseCase f d)

-- Abs a para devolver el rec, tenes rama izquierda y rama derecha
recAb :: b -> (b -> a -> b -> AB a -> AB a -> b) -> AB a -> b
recAb baseCase f Nil = baseCase
recAb baseCase f (Bin i v d) = f (recAb baseCase f i) v (recAb baseCase f d) i d

esNil :: AB a -> Bool
esNil Nil = True
esNil _ = False

altura :: AB a -> Int
altura = foldAb 0 (\i v d -> 1 + max i d)

cantNodos :: AB a -> Int
cantNodos = foldAb 0 (\i v d -> 1 + i + d)

-- Conviene usar recr para poder acceder a la "cola del arbol" osea los hijos de abajo
esABB :: Ord a => AB a -> Bool
esABB = recAb True evaluar
    where
        evaluar recI v recD arbolIzquierdo arbolDerecho =
            case (esNil arbolIzquierdo, esNil arbolDerecho) of
                (True, True) -> True
                (False, True) -> recI && v > valorNodo arbolIzquierdo
                (True, False) -> recD && v < valorNodo arbolDerecho
                (False, False) -> recI && recD && v > valorNodo arbolIzquierdo && v < valorNodo arbolDerecho
        valorNodo (Bin _ v _) = v

-- Asumo que tiene al menos un elemento
-- Conviene usar recr para poder acceder a la "cola del arbol" osea los hijos de abajo
mejorSegunAb :: (a -> a -> Bool) -> AB a -> a
mejorSegunAb f arbol = recAb (valorNodo arbol) (evaluarBase f) arbol
    where
        valorNodo (Bin _ v _) = v
        evaluarBase f recI v recD arbolIzquierdo arbolDerecho =
            case (esNil arbolIzquierdo, esNil arbolDerecho) of
                (True, True) -> v
                (False, True) -> if f v recI then v else recI
                (True, False) -> if f v recD then v else recD
                (False, False) -> evaluarMejor f recI v recD
        evaluarMejor f recI v recD =
            case (f recI v && f recI recD, f v recI && f v recD, f recD v && f recD recI) of
                (True, False, False) -> recI
                (False, True, False) -> v
                (False, False, True) -> recD



-- Ejercicio 14
-- Caso base: No tiene hijos. Idea: Agrrar elementos hasta llegar a la raiz que ahi es porque termino mi rama. Problema: No se de que lado vengo para ver si tengo que devolver el arbol izquierdo o el arbol derecho.
--ramas :: AB a -> [[a]]
--ramas (Bin i v d) = recAb v (\recI vA recD aIzq aDer -> if not (tieneHijos aIzq) && not (tieneHijos aDer)  then
--                            [vA] else recI : [recD])
--                            (Bin i v d)
--    where
--        tieneHijos Nil = False
--        tieneHijos (Bin _ _ _) = True

-- i y d son los resultados de aplicar la recursion
cantHojas :: AB a -> Int
cantHojas = recAb 0 (\recI v recD aIzq aDer -> if not (tieneHijos aIzq) && not (tieneHijos aDer)
                    then 1 else recI + recD)
    where
        tieneHijos Nil = False
        tieneHijos (Bin _ _ _) = True


espejo :: AB a -> AB a
espejo = foldAb Nil (\i v d -> Bin d v i)

-- Preguntar si esta bien asi o hace falta hacer doble fold
mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura Nil Nil = True
mismaEstructura (Bin ia va da) (Bin ib vb db)
    | cantNodos (Bin ia va da) /= cantNodos (Bin ib vb db)  = False
    | otherwise = mismaEstructura ia ib  && mismaEstructura da db




-- Ejercicio 15

data AIH a = Hoja a | Bin2 (AIH a) (AIH a)

foldAih :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAih fHoja fRecu  (Hoja a) = fHoja a
foldAih fHoja fRecu (Bin2 binI binD) = fRecu (foldAih fHoja fRecu binI) (foldAih fHoja fRecu binD)

alturaAih :: AIH a -> Integer
alturaAih = foldAih (const 0) (\recI recD -> 1 + max recI recD)

sizeAih :: AIH a -> Integer
sizeAih = foldAih (const 1) (\recI recD -> recI + recD)

-- Ejercicio 16

data RoseTree a = Rose a [RoseTree a]

-- A es el valor del nodo actual, lista de b el valor de aplicar la funcion sobre sus hijos
foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose x hijos) = f x (map (foldRose f) hijos)


-- Como hago para determinar la ultima hoja? Porque hablar sobre hijos es hablar sobre el resultado de aplicar la funcion a hijos, y eso no me sirve
--hojas :: RoseTree a -> [a]
--hojas = foldRose (\actual hijos -> ) 


--distancias :: RoseTree a -> Int


alturaRt :: RoseTree a -> Int
alturaRt = foldRose (\actual hijos -> 1 + maximum (1 : hijos))

arbolAltura5 :: AB Int
arbolAltura5 =
    Bin
        (Bin
            (Bin
                (Bin Nil 1 Nil)
                2
                (Bin Nil 3 Nil))
            4
            (Bin
                (Bin Nil 5 Nil)
                6
                (Bin Nil 7 Nil)))
        8
        (Bin
            (Bin Nil 9 Nil)
            10
            (Bin
                (Bin Nil 11 Nil)
                12
                (Bin
                    Nil
                    13
                    (Bin Nil 14 Nil))))

arbol1 :: AB Int
arbol1 =
    Bin
        (Bin (Bin Nil 0 Nil) 1 Nil)
        2
        (Bin Nil 3 Nil)

arbol2 :: AB Char
arbol2 =
    Bin
        (Bin Nil 'a' Nil)
        'b'
        (Bin Nil 'c' (Bin Nil 'd' Nil))


arbolBinario :: AB Int
arbolBinario =
    Bin
        (Bin
            (Bin Nil 1 Nil)
            2
            (Bin Nil 3 Nil)
        )
        4
        (Bin
            (Bin Nil 5 Nil)
            6
            (Bin Nil 7 Nil)
        )

arbolNoBinario :: AB Int
arbolNoBinario =
    Bin
        (Bin
            Nil
            3
            (Bin Nil 2 Nil)
        )
        5
        (Bin
            (Bin Nil 6 Nil)
            4
            Nil
        )

aih :: AIH Int
aih =
    Bin2
        (Bin2
            (Hoja 1)
            (Bin2
                (Hoja 3)
                (Hoja 4)
            )
        )
        (Hoja 2)


aih2 :: AIH Int
aih2 =
    Bin2
        (Bin2
            (Bin2
                (Hoja 1)
                (Bin2
                    (Hoja 4)
                    (Bin2
                        (Hoja 7)
                        (Hoja 8)
                    )
                )
            )
            (Bin2
                (Hoja 2)
                (Hoja 5)
            )
        )
        (Bin2
            (Hoja 3)
            (Hoja 6)
        )

roseTreeEjemplo :: RoseTree Int
roseTreeEjemplo =
    Rose 1 [
        Rose 2 [
            Rose 3 [],
            Rose 4 []
        ],
        Rose 5 [
            Rose 6 [],
            Rose 7 []
        ],
        Rose 8 [
            Rose 9 [],
            Rose 10 []
        ]
    ]

main :: IO ()
main = do
    print (alturaAih aih2)


-- Todo: 
--  4 I, IV
--  9 II
--  12
--  14 I
--  16 I, II