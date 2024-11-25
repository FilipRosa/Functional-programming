data TernaryTree a = Empty
                   | Node a (TernaryTree a) (TernaryTree a) (TernaryTree a) deriving (Show)

tree1 :: TernaryTree Int
tree1 = Node 1 (Node 2 Empty Empty Empty) (Node 3 Empty Empty Empty) (Node 4 Empty Empty Empty)

data Component = TextBox {name :: String, text :: String}
               | Button {name :: String, value :: String}
               | Container {name :: String, children :: [Component]} deriving (Show)

gui :: Component
gui = Container "My App" [
        Container "Menu" [
            Button "btn_new" "New",
            Button "btn_open" "Open",
            Button "btn_close" "Close"],
        Container "Body" [TextBox "textbox_1" "Some text goes here"],
        Container "Footer" []]

countOccurencesOfEachComponent :: Component -> (Int, Int, Int)
countOccurencesOfEachComponent (Button _ _) = (0, 1, 0)
countOccurencesOfEachComponent (TextBox _ _) = (1, 0, 0)
countOccurencesOfEachComponent (Container _ children) = foldl (\(b, t, c) children -> let (b', t', c') = countOccurencesOfEachComponent children in (b + b', t + t', c + c')) (0, 0, 1) children

addComponentToContainerAtIndex :: Component -> Component -> String -> Int -> Component
addComponentToContainerAtIndex newComp (Container n children) target index | n == target = Container n (insertAtIndex children newComp index)
                                                                           | otherwise = Container n (map (\child -> addComponentToContainerAtIndex newComp child target index) children)
addComponentToContainerAtIndex _ component _ _ = component

insertAtIndex children newComp index | index < 0 = children
                                     | index >= length children = children ++ [newComp]
                                     | otherwise = let (before, after) = splitAt index children in before ++ [newComp] ++ after