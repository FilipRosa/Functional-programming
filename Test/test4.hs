type Maze = [String]

ms :: Maze
ms = [ "*.......",
       ".....#..",
       "..*.*#..",
       "..*.*#..",
       "..***#..",
       ".#####..",
       "......*.",
       "........"]

printMaze :: Maze -> IO ()
printMaze x = putStr (concat (map (++ "\n") x))

getFromMaze :: Maze -> (Int, Int) -> Char
getFromMaze m (x, y) = (m !! x) !! y

putIntoMaze :: Maze -> [(Int, Int, Char)] -> Maze
putIntoMaze maze updates = foldl applyUpdate maze updates where
    applyUpdate :: Maze -> (Int, Int, Char) -> Maze
    applyUpdate maze (row, col, char) = let (beforeRow, targetRow:afterRow) = splitAt row maze
                                            updatedRow = take col targetRow ++ [char] ++ drop (col + 1) targetRow
                                        in beforeRow ++ [updatedRow] ++ afterRow

getNeighbours (r, c) = [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1), (r, c - 1), (r, c + 1), (r + 1, c - 1), (r + 1, c), (r + 1, c + 1)]

minesInMaze :: Maze -> Maze
minesInMaze maze = [[countMines (r, c) | c <- [0..length row - 1]] | (r, row) <- zip [0..] maze] where
    countMines (r, c) | getFromMaze maze (r, c) == '*' = '*'
                      | getFromMaze maze (r, c) == '#' = '#'
                      | otherwise = let mines = length [() | (nr, nc) <- getNeighbours (r, c), inBounds nr nc, getFromMaze maze (nr, nc) == '*']
                                    in if mines == 0 then '.' else head (show mines)

    inBounds r c = r >= 0 && r < length maze && c >= 0 && c < length (maze !! r)