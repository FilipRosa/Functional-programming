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

printMaze :: Maze -> IO ()
printMaze x = putStr (concat (map (++ "\n") x))

putIntoMaze :: Maze -> [(Int, Int, Char)] -> Maze
putIntoMaze maze updates = foldl applyUpdate maze updates where
    applyUpdate :: Maze -> (Int, Int, Char) -> Maze
    applyUpdate maze (row, col, char) = let (beforeRow, targetRow:afterRow) = splitAt row maze
                                            updatedRow = take col targetRow ++ [char] ++ drop (col + 1) targetRow
                                        in beforeRow ++ [updatedRow] ++ afterRow

getPath (row, column) ch | ch == 'd' = (row + 1, column)
                         | ch == 'u' = (row - 1, column)
                         | ch == 'l' = (row, column - 1)
                         | ch == 'r' = (row, column + 1)

changeDirection (r1, c1) (r2, c2) (r3, c3) = (r1 == r2 && r2 /= r3) || (c1 == c2 && c2 /= c3)

findChar (r1, c1) (r2, c2) (r3, c3) | r1 == r3 && c1 == c3 = '+'
                                    | changeDirection (r1, c1) (r2, c2) (r3, c3) = '+'
                                    | r1 == r2 = '-'
                                    | c1 == c2 = '|'
                                    | otherwise = '+'

markPath :: Maze -> (Int, Int) -> [Char] -> Maze
markPath maze (startRow, startColumn) path = let
    indexes = scanl getPath(startRow, startColumn) path
    pathPosition = tail indexes
    updates = [(r, c, 'x') | (r, c) <- pathPosition]
    startUpdate = [(startRow, startColumn, 'x')]
    in putIntoMaze maze (startUpdate ++ updates)