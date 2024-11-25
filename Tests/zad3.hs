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

countAllComponents :: Component -> Int
countAllComponents (TextBox _ _) = 1
countAllComponents (Button _ _) = 1
countAllComponents (Container _ child) = 1 + sum (map countAllComponents child)

removeEmptyContainers :: Component -> Component
removeEmptyContainers (TextBox n t) = TextBox n t
removeEmptyContainers (Button n t) = Button n t
removeEmptyContainers (Container n child) | null child = Container n []
                                          | otherwise = Container n (map removeEmptyContainers child)