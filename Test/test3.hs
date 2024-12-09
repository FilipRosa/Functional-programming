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

getPart :: Maze -> (Int, Int) -> (Int, Int) -> Maze
getPart maze (startRow, startCol) (height, width) = take height (map (take width . drop startCol) (drop startRow maze))

minimaps :: Maze -> (Int, Int) -> [(Char, Int)] -> Maze
minimaps maze startPos directions = concatMap createMinimap positionsWithTurns where
    positionsWithTurns = scanl move startPos directions

    move (r, c) (dir, len) = let (deltaRow, deltaCol) = getDirections dir
                             in (r + deltaRow * len, c + deltaCol * len)
        
    getDirections dir | dir == 'd' = (1, 0)
                      | dir == 'u' = (-1, 0)
                      | dir == 'l' = (0, -1)
                      | dir == 'r' = (0, 1)

    createMinimap (r, c) = let minimap = getPart maze (r - 1, c - 1) (3, 3)
                           in printMinimap minimap

    printMinimap minimap = ["---"] ++ minimap ++ ["---"]        