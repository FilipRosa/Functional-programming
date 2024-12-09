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

getNeighbours (ri, ci) = [(ri - 1, ci),(ri + 1, ci), (ri, ci - 1), (ri, ci + 1)]

solve [] _ = []
solve ((ri, ci, price):toSolve) nowEmpty = let
    allNeighbours = [n | n <- getNeighbours (ri, ci), elem n nowEmpty]
    newEmpty = [n | n <- nowEmpty, notElem n allNeighbours]
    in (ri, ci, price) : solve (toSolve ++ [(r, c, price + 1) | (r, c) <- allNeighbours]) newEmpty

solveMaze :: Maze -> Int
solveMaze maze = let
    indexes = concat[[(ri, ci, ch) | (ci, ch) <- zip[0..] line] | (ri, line) <- zip[0..] maze]
    (sr, sc) = head[(ri, ci) | (ri, ci, ch) <- indexes, ch == 's']
    (er, ec) = head[(ri, ci) | (ri, ci, ch) <- indexes, ch == 'e']
    empty = [(ri, ci) | (ri, ci, ch) <- indexes, ch == ' ' || ch == 'e']
    solved = solve [(sr, sc, 0)] empty
    in head [price | (r, c, price) <- solved, r == er, c == ec]

solve' [] _ = []
solve' ((ri, ci, price):toSolve) nowEmpty = let
    allNeighbours = [n | n <- getNeighbours (ri, ci), elem n nowEmpty]
    newEmpty = [n | n <- nowEmpty, notElem n allNeighbours]
    updatedSolve = toSolve ++ [(r, c, price + 1) | (r, c) <- allNeighbours]
    uniqueSolve = filter (\(r, c, p) -> notElem (r, c) (map (\(xr, xc, _) -> (xr, xc)) toSolve)) updatedSolve
    in (ri, ci, price) : solve' (toSolve ++ uniqueSolve) newEmpty
                                         
makePath :: Maze -> (Int, Int) -> (Int, Int) -> Maze
makePath maze (sr, sc) (er, ec) = let
    indexes = concat[[(ri, ci, ch) | (ci, ch) <- zip[0..] line] | (ri, line) <- zip[0..] maze]
    empty = [(ri, ci) | (ri, ci, ch) <- indexes, ch == ' ']
    solved = solve' [(sr, sc, 0)] empty
    in putIntoMaze maze [(r, c, 'x') | (r, c, _) <- solved]
    
