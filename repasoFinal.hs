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
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

foldrRecr :: (a -> b -> b) -> b -> [a] -> b
foldrRecr f = recr (\x _ rec -> f x rec)

recrFoldr :: 

main :: IO ()
main = do
    print (" Hola PLP")