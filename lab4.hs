import Data.Char

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' 0 x = x
drop' _ [] = []
drop' n (x:xs) = drop' (n - 1) xs

minimum' :: Ord a => [a] -> a
minimum' [x] = x
minimum' (x:y:z) | x < y = minimum'(x:z)
                 | otherwise = minimum'(y:z)

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

zipThem :: [a] -> [b] -> [(a,b)]
zipThem (x:xs) (y:ys) = (x,y) : zipThem xs ys
zipThem _ _ = []

dotProduct :: [a] -> [b] -> [(a,b)]
dotProduct xs ys = [(x,y) | x <- xs, y <- ys]

fibonacci :: Int -> Int
fibonacci n = fst (tmp n) where
    fibStep (a,b) = (b, a + b)
    tmp 0 = (0,1)
    tmp x = fibStep (tmp (x-1))

allToUpper :: String -> String
allToUpper xs = [toUpper x | x <- xs]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let lp = filter (<x) xs
                       rp = filter (>= x) xs
                    in quicksort lp ++ [x] ++ quicksort rp

removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne n (x:xs) | x == n = xs
                   | otherwise = x : removeOne n xs

removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll n (x:xs) | x == n = removeAll n xs
                   | otherwise = x : removeAll n xs

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

alternate :: [a] -> [a] -> [a]
alternate [] ys = ys
alternate xs [] = xs
alternate (x:xs) (y:ys) = x : y : alternate xs ys

filterEvenGt7 :: [Int] -> [Int]
filterEvenGt7 (x:xs) = filter (>= 7) xs

makeTriples :: [a] -> [(a,a,a)]
makeTriples (x:y:z:xs) = (x,y,z) : makeTriples xs
makeTriples _ = []

insertOnIndex :: [a] -> a -> Int -> [a]
insertOnIndex xs n 0 = n : xs
insertOnIndex (x:xs) n v = x : insertOnIndex xs n (v - 1)
insertOnIndex [] n _ = [n]

join :: [[a]] -> a -> [a]
join [] _ = []
join [x] _ = x
join (x:xs) sep = x ++ [sep] ++ join xs sep