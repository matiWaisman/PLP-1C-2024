-- Ejercicio 1

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

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


-- No anda
generateBase::Eq a => ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop baseCase f = generate (\ls -> last ls == baseCase && length ls > 1) (\ls -> if null ls then baseCase else f (last ls))

factorial :: Int -> Int
factorial n = foldr (\x acc-> x * acc) 1 [1..n]

factoriales::Int -> [Int]
factoriales n = generate (\ls -> not (null ls) && (last ls > factorial n)) (\ls -> if null ls then 1 else last ls * (length ls + 1))

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

main :: IO ()
main = do
    print (potencia 2 3)
