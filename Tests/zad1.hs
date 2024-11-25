data DynamicTree a = Node a [DynamicTree a]
                   | Leaf a deriving (Show)

tree1 :: DynamicTree Int
tree1 = Node 1 [Leaf 2, Node 3 [Leaf 4, Leaf 5], Leaf 6]

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

guiDepth :: Component -> Int
guiDepth (TextBox _ _) = 1
guiDepth (Button _ _) = 1
guiDepth (Container _ child) | null child = 1
                             | otherwise = 1 + maximum (map guiDepth child)

justContainer :: Component -> Component
justContainer (TextBox _ _) = Container "" []
justContainer (Button _ _) = Container "" []
justContainer (Container n child) = Container n (map justContainer child)