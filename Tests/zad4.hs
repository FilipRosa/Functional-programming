data Attribute = Attribute String String deriving (Show)

data Tag = Tag {tagName :: String, attributes :: [Attribute], nestedTags :: [Tag]} deriving (Show)

data HTMLDocument = HTMLDocument [Tag] deriving (Show)

attr1 = Attribute "class" "header"
attr2 = Attribute "id" "main-header"
attr3 = Attribute "href" "https://google.com"

headerTag = Tag "h1" [attr1, attr2] []

linkTag = Tag "a" [attr3] []

bodyTag = Tag "body" [] [headerTag, linkTag]

document = HTMLDocument [bodyTag]

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

listButtonNames :: Component -> [String]
listButtonNames (Button n _) = [n]
listButtonNames (Container _ child) = concatMap listButtonNames child
listButtonNames _ = []

changeText :: Component -> String -> String -> Component
changeText (TextBox n t) target newText | t == target = TextBox n newText
                                        | otherwise = TextBox n t
changeText (Container n child) target newText = Container n (map (\child -> changeText child target newText) child)
changeText x _ _ = x