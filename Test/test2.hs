import Data.List (minimumBy)
import Data.Function (on)

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

printMaze :: Maze -> IO ()
printMaze x = putStr (concat (map (++ "\n") x))

putIntoMaze :: Maze -> [(Int, Int, Char)] -> Maze
putIntoMaze maze updates = foldl applyUpdate maze updates where
    applyUpdate :: Maze -> (Int, Int, Char) -> Maze
    applyUpdate maze (row, col, char) = let (beforeRow, targetRow:afterRow) = splitAt row maze
                                            updatedRow = take col targetRow ++ [char] ++ drop (col + 1) targetRow
                                        in beforeRow ++ [updatedRow] ++ afterRow

solveNum maze (x, y) end seen delka =
    let above = if x - 1 >= 0 then (maze !! (x - 1)) !! y else '*'
        below = if x + 1 < length maze then (maze !! (x + 1)) !! y else '*'
        left  = if y - 1 >= 0 then (maze !! x) !! (y - 1) else '*'
        right = if y + 1 < length (maze !! x) then (maze !! x) !! (y + 1) else '*'
        test_above = if (x - 1, y) == end then end : (x, y) : seen
                     else if above == ' ' && (x - 1, y) `notElem` seen then solveNum maze (x - 1, y) end ((x, y) : seen) (delka + 1)
                     else []
        test_below = if (x + 1, y) == end then end : (x, y) : seen
                     else if below == ' ' && (x + 1, y) `notElem` seen then solveNum maze (x + 1, y) end ((x, y) : seen) (delka + 1)
                     else []
        test_left = if (x, y - 1) == end then end : (x, y) : seen
                    else if left == ' ' && (x, y - 1) `notElem` seen then solveNum maze (x, y - 1) end ((x, y) : seen) (delka + 1)
                    else []
        test_right = if (x, y + 1) == end then end : (x, y) : seen
                     else if right == ' ' && (x, y + 1) `notElem` seen then solveNum maze (x, y + 1) end ((x, y) : seen) (delka + 1)
                     else []
    in if (delka > (length maze * length (maze !! 0))) || all null [test_above, test_below, test_left, test_right]
       then []
       else minimumBy (compare `on` length) [n | n <- [test_above, test_below, test_left, test_right], not (null n)]

makePath :: Maze -> (Int, Int) -> (Int, Int) -> Maze
makePath maze start end = let path = reverse (solveNum maze start end [] 0)
                          in putIntoMaze maze [(fst (path !! i), snd (path !! i), head (show (i `mod` 10))) | i <- [0..length path -1]]