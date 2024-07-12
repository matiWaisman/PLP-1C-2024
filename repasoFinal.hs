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

zipFold :: [a] -> [b] -> [(a,b)]
zipFold = foldr (\x rec ys -> if (not (null ys)) then (x, head ys) : rec (tail ys) else []) (const [])

-- Definir foldr en base a recr
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

foldrRecr :: (a -> b -> b) -> b -> [a] -> b
foldrRecr f = recr (\x _ rec -> f x rec)

-- Definir recr en base a foldr
recrFoldr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recrFoldr f z l = foldr (\x rec -> \l -> f x l (rec (tail l))) (\_ -> z) l l 

-- Prueba de que funciona bien 
trimP :: String -> String
trimP = recrFoldr (\ x xs rec -> if x == ' ' then rec else x : xs) []

-- Definir foldl en base a foldr
foldlConFoldr f rec l = foldr g rec (reverse l)
    where g e rec = f rec e

-- Prueba de que funciona bien 
bin2dec :: [Int] -> Int
bin2dec = foldlConFoldr (\ ac b -> b + 2 * ac) 0

-- Definir foldr en base a foldl
foldrConFoldl f rec l = foldl g rec (reverse l)
    where g rec e = f e rec

entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x rec ys -> if null ys then x : rec ys else x : head ys : rec (tail ys)) id

permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x rec -> concatMap (\rs -> permutacionesConUnElemento x rs) rec) [[]]

permutacionesConUnElemento :: a -> [a] -> [[a]]
permutacionesConUnElemento e l = foldr (\i rec -> (take i l ++ [e] ++ drop i l) : rec) [] [0.. length l]

partes :: [a] -> [[a]]
partes = foldl (\rec x -> rec ++ map (\e -> e ++ [x]) rec) [[]]

prefijos :: [a] -> [[a]]
prefijos = foldl (\rec x -> rec ++ [last rec ++ [x]]) [[]]

sufijos :: [a] -> [[a]]
sufijos l = map reverse (prefijos (reverse l))

sublistas :: [a] -> [[a]]
sublistas = recr (\x xs rec -> map (x:) (prefijos xs) ++ rec) [[]]

main :: IO ()
main = do
    print (partes [5,1,2])