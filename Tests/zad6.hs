data Article = ArticleText String
             | Section {name2 :: String, uSection :: [Article], text2 :: [String]} deriving (Show)

exampleArticle :: Article
exampleArticle = Section "Introduction" [ArticleText "Intro text"] ["Overview"]

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

listAllNames :: Component -> [String]
listAllNames (TextBox n _) = [n]
listAllNames (Button n _) = [n]
listAllNames (Container n child) = n : concatMap listAllNames child

removeElements :: Component -> [String] -> Component
removeElements (TextBox n v) target | n `elem` target = Container "" []
                                    | otherwise = TextBox n v

removeElements (Button n v) target | n `elem` target = Container "" []
                                   | otherwise = Button n v

removeElements (Container n child) target = Container n (map (`removeElements` target) child)