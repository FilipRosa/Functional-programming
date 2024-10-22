length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

sumIt :: [Int] -> Int
sumIt [] = 0
sumIt (x:xs) = x + sumIt xs

getHead :: [a] -> a
getHead (x:_) = x

getLast :: [a] -> a
getLast (x:xs) | length' xs == 0 = x
               | otherwise = getLast xs

isElement :: Eq a => a -> [a] -> Bool
isElement n [] = False
isElement n (x:xs) | n == x = True
                   | otherwise = isElement n xs

getTail :: [a] -> [a]
getTail (_:xs) = xs

getInit :: [a] -> [a]
getInit [_] = []
getInit (x:xs) = x : getInit xs

combine :: [a] -> [a] -> [a]
combine [] y = y
combine (x:xs) y = x : combine xs y

max' :: [Int] -> Int
max' [x] = x
max' (x:y:z) | x > y = max' (x:z)
             | otherwise = max' (y:z)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

scalar :: [Int] -> [Int] -> Int
scalar [] [] = 0
scalar (x:xs) (y:ys) = x * y + scalar xs ys

nonZeros :: [Int] -> [Int]
nonZeros [] = []
nonZeros (x:xs) | x == 0 = nonZeros xs
                | otherwise = x : nonZeros xs

rotateLeft1 :: [a] -> [a]
rotateLeft1 [] = []
rotateLeft1 (x:xs) = xs ++ [x]

rotateRight1 :: [a] -> [a]
rotateRight1 [] = []
rotateRight1 [x] = [x]
rotateRight1 (x:xs) = tmp x xs where
    tmp n [] = [n]
    tmp n (y:ys) = y : tmp n ys

oddMembers :: [Int] -> [Int]
oddMembers [] = []
oddMembers (x:xs) | isOdd x = x : oddMembers xs
                  | otherwise = oddMembers xs where
                    isOdd n = n `mod` 2 /= 0

countOddMemebers :: [Int] -> Int
countOddMemebers xs = length' (oddMembers xs)

compareLists :: Eq a => [a] -> [a] -> Bool
compareLists [] [] = True
compareLists [] _ = False
compareLists _ [] = False
compareLists (x:xs) (y:ys) | x == y = compareLists xs ys
                           | otherwise = False