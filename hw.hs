type Maze = [String]

sample1 :: Maze
sample1 = ["*********",
           "* *   * *",
           "* * * * *",
           "* * * * *",
           "*   *   *",
           "******* *",
           "        *",
           "*********"]

sample2 :: Maze
sample2 = ["       ",
           "       ",
           "  ***  ",
           "  ***  ",
           "  ***  ",
           "       ",
           "       "]

sample3 :: Maze
sample3 = ["  * *  ",
           " ##### ",
           "  ***  ",
           "  * *  ",
           "  ***  ",
           "     * ",
           "       "]

sample4 :: Maze
sample4 = ["*********",
           "*s*   *e*",
           "* *   * *",
           "* *   * *",
           "*       *",
           "******* *",
           "        *",
           "*********"]

arrow :: Maze
arrow = [ "....#....",
          "...###...",
          "..#.#.#..",
          ".#..#..#.",
          "....#....",
          "....#....",
          "....#####"]

printMaze :: Maze -> IO ()
printMaze x = putStr (concat (map (++ "\n") x))

above :: Maze -> Maze -> Maze
above x y = x ++ y

sideBySide :: Maze -> Maze -> Maze
sideBySide = zipWith (++)

toRow :: String -> Maze
toRow xs = map (\x -> [x]) xs

rotateR :: Maze -> Maze
rotateR x = foldl1 sideBySide (reverse (map toRow x))

rotateL :: Maze -> Maze
rotateL x = foldl1 sideBySide (map (reverse.toRow) x)

getFromMaze :: Maze -> (Int, Int) -> Char
getFromMaze m (x, y) = (m !! x) !! y

putIntoMaze :: Maze -> [(Int, Int, Char)] -> Maze
putIntoMaze maze updates = foldl applyUpdate maze updates where
    applyUpdate :: Maze -> (Int, Int, Char) -> Maze
    applyUpdate maze (row, col, char) = let (beforeRow, targetRow:afterRow) = splitAt row maze
                                            updatedRow = take col targetRow ++ [char] ++ drop (col + 1) targetRow
                                        in beforeRow ++ [updatedRow] ++ afterRow

getPart :: Maze -> (Int, Int) -> (Int, Int) -> Maze
getPart maze (startRow, startCol) (height, width) = take height (map (take width . drop startCol) (drop startRow maze))