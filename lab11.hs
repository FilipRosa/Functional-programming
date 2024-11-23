{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Eta reduce" #-}
import Distribution.Simple.Utils (xargs)
data Point = Point {column :: Int, row :: Int} deriving (Show)

data Position = Position {leftTopCorner :: Point, width :: Int, height :: Int}

data Event = MouseEvent Point
           | KeyEvent {keyPressed :: Char} deriving (Show)

data Component = TextBox {name :: String, position :: Position, text :: String}
               | Button {name :: String, position :: Position, text :: String, onClick :: Maybe (Event -> String)}
               | Container {name :: String, children :: [Component]}

gui :: Component
gui =
    Container "My App"
        [Container "Menu"
            [Button "btn_new" (Position (Point 0 0) 100 20) "New" (Just (\event -> "Clicked on new button.")),
            Button "btn_new" (Position (Point 100 0) 100 20) "Open" Nothing,
            Button "btn_close" (Position (Point 200 0) 100 20) "Close" (Just (\event -> "Clicked on close button."))
            ],
        Container "Body" [TextBox "textbox_1" (Position (Point 0 20) 300 500) "Some text goes here"],
        Container "Footer" []
        ]

instance Show Position where
    show (Position (Point col row) width height) = "(" ++ show col ++ "," ++ show row ++ ")[" ++ show width ++ "," ++ show height ++ "]"

instance Show Component where
    show :: Component -> String
    show gui = showIndent "" gui where
        showIndent ind (TextBox name position text) = ind ++ show position ++ " TextBox[" ++ name ++ "]: " ++ text ++ "\n"
        showIndent ind (Button name position text onClick) = ind ++ show position ++ " Button[" ++ name ++ "]: " ++ text ++ "\n"
        showIndent ind (Container name children) = let inner = concat[showIndent (ind ++ "\t") c | c <- children]
                                                   in ind ++ "Container - " ++ name ++ "\n" ++ inner

insertInto :: Component -> String -> Component -> Component
insertInto (Container cName children) toName element | cName == toName = Container cName (children ++ [element])
                                                     | otherwise = Container cName [insertInto c toName element | c <- children]
insertInto x toName element = x

deleteFrom :: Component -> String -> Component
deleteFrom (Container x child) target = Container x [deleteFrom c target | c <- child, name c /= target]
deleteFrom c _ = c

isInside :: Point -> Position -> Bool
isInside (Point pCol pRow) (Position (Point cornerCol cornerRow) width height) = cornerCol <= pCol && pCol <= cornerCol + width && cornerRow <= pRow && pRow <= cornerRow + height

getFirstOrNothing :: [Maybe a] -> Maybe a
getFirstOrNothing [] = Nothing
getFirstOrNothing (Nothing:xs) = getFirstOrNothing xs
getFirstOrNothing (Just x:xs) = Just x

clickOnButton :: Component -> Event -> Maybe String
clickOnButton (Button {position = pos, onClick = (Just func)}) (MouseEvent point) | isInside point pos = Just (func (MouseEvent point))
clickOnButton (Container {children = inner}) event = getFirstOrNothing [clickOnButton c event | c <- inner]
clickOnButton _ _ = Nothing

data MTree a = MTree a [MTree a]

testTree1 :: MTree Int
testTree1 = MTree 1 [(MTree 2 [(MTree 3 []), (MTree 4 [(MTree 5 []), (MTree 6 [])]), (MTree 7 []), (MTree 8 [])]), (MTree 9 [])]

msum :: MTree Int -> Int
msum (MTree value subt) = value + sum (map msum subt)

mToList :: MTree a -> [a]
mToList (MTree value subt) = value : concatMap mToList subt

mLeafCount :: MTree a -> Int
mLeafCount (MTree _ []) = 1
mLeafCount (MTree _ subt) = sum (map mLeafCount subt)

mMaxTree :: Ord a => MTree a -> a
mMaxTree (MTree value []) = value
mMaxTree (MTree value subt) = let maxSubtree = maximum (map mMaxTree subt)
                              in max value maxSubtree

mContains :: Eq a => MTree a -> a -> Bool
mContains (MTree value subt) target | value == target = True
                                    | otherwise = any (`mContains` target) subt

mGreatherThan :: Ord a => MTree a -> a -> Int
mGreatherThan (MTree value subt) target | value > target = 1 + sum (map (`mGreatherThan` target) subt)
                                        | otherwise = sum (map (`mGreatherThan` target) subt)