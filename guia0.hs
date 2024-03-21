-- Ejercicio 2

valorAbsoluto :: Float -> Float
valorAbsoluto x | x > 0     = x
                | otherwise = -x

esBisiesto :: Int -> Bool
esBisiesto a | (a `mod` 4 == 0) &&  (a `mod` 100 /= 0) = True
             | a `mod` 400 == 0  = True
             | otherwise = False

factorial :: Int -> Int
factorial 0 = 1
factorial n = factorial (n-1) * n

-- Solo funciona con numeros positivos
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos 1 = 0
cantDivisoresPrimos n = length divisoresPrimos
    where divisoresPrimos = filter (\x -> esPrimo x && n `mod` x == 0) [2..n]

esPrimo :: Int -> Bool
esPrimo n = not (any (\x -> n `mod` x == 0) posiblesDivisores)
    where posiblesDivisores = [2.. floor (sqrt (fromIntegral n))]


-- Ejercicio 3 

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso a = Just (1 / a)

aEntero :: Either Int Bool -> Int
aEntero (Left n) = n
aEntero (Right b)
    | b  = 1
    | otherwise = 0


-- Ejercicio 4

limpiar :: String -> String -> String -- Con la lamda function recorre los caracteres de palabra, y si encuentra que el caracter pertenece a filtro no lo agrega a palabra. 
limpiar filtro palabra = filter (\x -> not (elem x filtro)) palabra

difPromedio :: [Float] -> [Float]
difPromedio l = map (\x -> x - promedio) l
    where promedio = sumarElementos l / fromIntegral (length l)

sumarElementos :: [Float] -> Float
sumarElementos [] = 0
sumarElementos [x] = x
sumarElementos (x:xs) = sumarElementos xs + x


todosIguales :: [Int] -> Bool
todosIguales l
    | any (\x -> x /= head l) l = False
    | otherwise = True

-- Ejercicio 5

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin aIzq valor aDerecha) = Bin (negacionAB aIzq) (not valor) (negacionAB aDerecha)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin aIzq valor aDerecha) = productoAB aIzq * valor * productoAB aDerecha

-- Extra practica Merge Sort

mergeSort :: [Int] -> [Int]
mergeSort [x] = [x]
mergeSort l = merge (mergeSort primeraLista) (mergeSort segundaLista)
    where primeraLista = take (length l `div` 2) l
          segundaLista = drop (length l `div` 2) l

merge :: [Int] -> [Int] -> [Int]
merge l1 [] = l1
merge [] l2 = l2
merge (l1s : l1) (l2s : l2)
    | l1s < l2s = l1s : merge l1 (l2s : l2)
    | otherwise = l2s : merge (l1s : l1)  l2




