import Data.List (sortBy)

not' :: Bool -> Bool
not' x | x == True = False
       | otherwise = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' x y = not' (and' x y)

xor' :: Bool -> Bool -> Bool
xor' x y = x /= y

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' x y = x == y

table :: (Bool -> Bool -> Bool) -> IO()
table expr = putStr (concat [nicePrint [x, y, expr x y] | x <- [True, False], y <- [True, False]])

nicePrint :: [Bool] -> String
nicePrint xs = concat [show x ++ "\t" | x <- xs] ++ "\n"

tablen :: Int -> ([Bool] -> Bool) -> IO()
tablen n f = putStr (concat [nicePrint x ++ " => " ++ show(f x) ++ "\n" | x <- allValues n]) where
    allValues 1 = [[True], [False]]
    allValues n = [x:y | x <- [True, False], y <- allValues (n - 1)]

    nicePrint :: [Bool] -> String
    nicePrint xs = concat [show x ++ "\t" | x <- xs]

huffman :: [(Char, Int)] -> [(Char, String)]
huffman input = let prep = [(y, [(x,"")]) | (x, y) <- input]
                in sortBy (\ (x,_) (y,_) -> compare x y) (step prep) where
                    step :: [(Int, [(Char, String)])] -> [(Char, String)]
                    step [(_, result)] = result
                    step list = let ((a1, as2):(b1,bs2):rest) = sortBy (\ (x,_) (y,_) -> compare x y) list
                                in step ((a1 + b1, [(x, '0':a2) | (x, a2) <- as2] ++ [(x, '1':b2) | (x,b2) <- bs2]):rest)

splitByElement :: Eq a => [a] -> a -> [[a]]
splitByElement [] _ = []
splitByElement (x:xs) sep | x == sep = splitByElement xs sep
                          | otherwise = let (word, rest) = span (/= sep) (x:xs)
                                        in word : splitByElement rest sep