import System.IO
import Control.Exception

main = do fromHandle <- opf "Read from: " ReadMode
          contents <- hGetContents fromHandle
          putStr (numberLines contents)
          hClose fromHandle

opf :: String -> IOMode -> IO Handle
opf prompt mode = do putStr prompt
                     name <- getLine
                     catch (openFile name mode)
                           (\e -> do putStr ("Can't open " ++ name ++ ":" ++ show (e :: IOException) ++ "\n")
                                     opf prompt mode)

numberLines :: String -> String
numberLines text = let l = lines text
                       numbered = zip [1..] l
                       result = [show x ++ ".\t" ++ content ++ "\n" | (x, content) <- numbered]
                   in concat result 