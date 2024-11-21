type Pic = [String]

pp :: Pic -> IO()
pp x = putStr(unlines x)

pic :: Pic
pic = [ "....#....",
        "...###...",
        "..#.#.#..",
        ".#..#..#.",
        "....#....",
        "....#....",
        "....#####"]

flipV :: Pic -> Pic
flipV = map reverse

flipH :: Pic -> Pic
flipH = reverse

above :: Pic -> Pic -> Pic
above x y = x ++ y

sideBySide :: Pic -> Pic -> Pic
sideBySide = zipWith (++)

toRow :: String -> Pic
toRow xs = map (\x -> [x]) xs 

rotateR :: Pic -> Pic
rotateR x = foldl1 sideBySide (reverse (map toRow x))

rotateL :: Pic -> Pic
rotateL x = foldl1 sideBySide (map (reverse.toRow) x)

zoom :: Int -> Pic -> Pic
zoom n xs = [concatMap(replicate n) x | x <- concatMap(replicate n) xs]

pic2::Pic
pic2 = [ "#########",
         "#.......#",
         "#.......#",
         "#.......#",
         "#.......#",
         "#.......#",
         "#########"]

superimpose :: Pic -> Pic -> Pic
superimpose [] [] = []
superimpose (x:xs) (y:ys) = zipWith combine x y : superimpose xs ys where
                        combine '.' '.' = '.'
                        combine _ _ = '#'

invertColors :: Pic -> Pic
invertColors = map invertRow

invertRow :: String -> String
invertRow = map invertPixel

invertPixel :: Char -> Char
invertPixel '.' = '#'
invertPixel '#' = '.'

chessBoard :: Int -> Pic
chessBoard n = [generateRow i n | i <- [0..n-1]] where
    generateRow :: Int -> Int -> String
    generateRow i n = [if even (i + j) then '#' else '.' | j <- [0..n-1]]

makePicture :: Int -> Int -> [(Int, Int)] -> Pic
makePicture width height points = [[if (x, y) `elem` points then '#' else '.' | x <- [1..width]] | y <- [1..height]]