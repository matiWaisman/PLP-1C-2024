-- Ejercicio 3

sumaFoldr :: Num a => [a] -> a
sumaFoldr l = foldr (+) 0 l

elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr e = foldr (\x acc -> x == e || acc) False 

-- Agrega todos los elementos de l1 haciendo (:) y el caso base es agregar l2
concatFoldr :: [a] -> [a] -> [a]
concatFoldr l1 l2 = foldr (:) l2 l1

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr filtro l = foldr (\x acc -> if filtro x then x : acc else acc) [] l 

mapFoldr :: (a -> a) -> [a] -> [a]
mapFoldr funcion l = foldr(\x acc -> funcion x : acc) [] l

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun funcion l = foldr1(\x y ->  if funcion x y then x else y) l

sumasParciales :: Num a => [a] -> [a]
sumasParciales l =  foldl(\acc x -> x + acc) 0  l

sumaAlt :: Num a => [a] -> a
sumaAlt l = snd (foldr (\x (pos, acc) -> (pos + 1, if even pos then acc + x else acc - x)) (0, 0) (reverse l))

sumaAltInversa :: Num a => [a] -> a 
sumaAltInversa l = snd (foldr (\x (pos, acc) -> (pos + 1, if even pos then acc + x else acc - x)) (0, 0) l)


main :: IO ()
main = do
    print(sumaAlt [5, 2, 3, 4, 5, 6])