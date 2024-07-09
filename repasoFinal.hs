merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ l1 [] = l1
merge _ [] l2 = l2
merge p (e1 : xs1) (e2 : xs2) = if p e1 e2 then e1 : merge p xs1 (e2 : xs2) else e2 : merge p (e1 : xs1) xs2

mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort _ [e] = [e]
mergesort p l = merge p (mergesort p primeraLista) (mergesort p segundaLista)
    where primeraLista = take (length l `div` 2) l
          segundaLista = drop (length l `div` 2) l

zipE :: [a] -> [b] -> [(a, b)]
zipE [] [] = []
zipE (x : xs) (y : ys) = (x, y) : zipE xs ys

-- Definir foldr en base a recr
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

foldrRecr :: (a -> b -> b) -> b -> [a] -> b
foldrRecr f = recr (\x _ rec -> f x rec)

-- Definir recr en base a foldr
recrFoldr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recrFoldr f z (x : y:  xs) = f x xs (foldr (\_ rec -> f y xs rec) z xs)

recFoldrMalPorqueLoHizoFausto :: (a -> [a] -> b -> b) -> b -> [a] -> b
recFoldrMalPorqueLoHizoFausto f z xs = foldr (\x rec -> f x xs rec ) z xs

-- Prueba de que funciona bien 
trimP :: String -> String
trimP = recFoldrMalPorqueLoHizoFausto (\ x xs rec -> if x == ' ' then rec else x : xs) []

-- Definir foldl en base a foldr
foldlFoldr :: (b -> a -> b) -> b -> [a] -> b
foldlFoldr f rec l = foldr g rec (reverse l)
    where g e rec = f rec e

-- Prueba de que funciona bien 
bin2dec :: [Int] -> Int
bin2dec = foldlFoldr (\ ac b -> b + 2 * ac) 0

-- Definir foldr en base a foldl
foldrFoldl :: (a -> b -> b) -> b -> [a] -> b
foldrFoldl f rec l = foldl g rec (reverse l)
    where g rec e = f e rec

entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x rec ys -> if null ys then x : rec ys else x : head ys : rec (tail ys)) id

main :: IO ()
main = do
    print (entrelazar [1, 3, 5] [2, 4, 6])