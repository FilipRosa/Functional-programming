import Data.Char

oddList :: Int -> Int -> [Int]
oddList x y = [x | x <- [x..y], odd x]

removeAllUpper :: String -> String
removeAllUpper (x:xs) = [toLower x | x <- xs, not (isUpper x)]

union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ [y | y <- ys, not (elem y xs)]

intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys = [y | y <- ys, elem y xs]

unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

countThem :: String -> [(Char, Int)]
countThem xs = let u = unique xs
               in [(x, length (filter (==x) xs)) | x <- u]

isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..ceiling (sqrt (fromIntegral n)::Double)], n `mod` x == 0]

goldbach :: Int -> [(Int, Int)]
goldbach n = let primes = [x | x <- [2..(n `div` 2)], isPrime x]
             in [(x,n-x) | x <- primes, isPrime (n-x)]

combinations :: Int -> String -> [String]
combinations 1 xs = [[x] | x <- xs]
combinations n (x:xs) | n == length (x:xs) = [(x:xs)]
                      | otherwise = [[x] ++ y | y <- combinations (n-1) xs] ++ (combinations n xs)

foldrMap :: (a -> b) -> [a] -> [b]
foldrMap _ [] = []
foldrMap f (x:xs) = f x : foldrMap f xs

foldlConcatMap :: (a -> [b]) -> [a] -> [b]
foldlConcatMap f = foldl (\acc x -> acc ++ f x) []

partition :: (a -> Bool) -> [a] -> ([a],[a])
partition _ [] = ([],[])
partition pred (x:xs) | pred x = (x: yes, no)
                      | otherwise = (yes, x : no)
                      where (yes, no) = partition pred xs

split :: [(a,b)] -> ([a],[b])
split = foldr (\(x,y) (xs,ys) -> (x:xs, y:ys)) ([],[])

divideList :: [a] -> Int -> [[a]]
divideList [] _ = []
divideList xs n | length xs < n = []
                | otherwise = take n xs : divideList (drop n xs) n

sequences :: Ord a => [a] -> a -> [[a]]
sequences [] _ = []
sequences (x:xs) el | x > el = let (seq, rest) = span (> el) (x:xs)
                               in seq : sequences rest el
                    | otherwise = sequences xs el