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
todosIguales :: [Int] -> Bool
todosIguales l
    | any (\x -> x /= head l) l = False
    | otherwise = True







main :: IO ()
main =  do
    print (todosIguales [])
    print (todosIguales [1,2])
    print (todosIguales [1,1])
    print (todosIguales [1,1,1,3])
    print (todosIguales [5,5,5,5])
    print (todosIguales [5,4,4,5])

