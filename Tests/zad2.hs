data Entity = Point Double Double
            | Circle Double Double Int
            | Container [Entity] deriving (Show)

point :: Entity
point = Point 1.0 2.0

circle :: Entity
circle = Circle 1.0 2.0 5

container :: Entity
container = Container[point, circle]

data Component = TextBox {name :: String, text :: String}
               | Button {name :: String, value :: String}
               | Container2 {name :: String, children :: [Component]} deriving (Show)

gui :: Component
gui = Container2 "My App" [
        Container2 "Menu" [
            Button "btn_new" "New",
            Button "btn_open" "Open",
            Button "btn_close" "Close"],
        Container2 "Body" [TextBox "textbox_1" "Some text goes here"],
        Container2 "Footer" []]

countButtons :: Component -> Int
countButtons (Button _ _) = 1
countButtons (TextBox _ _) = 0
countButtons (Container2 _ comp) = sum (map countButtons comp)

addElement :: Component -> Component -> String -> Component
addElement (Container2 cName child) element toName | cName == toName = Container2 cName (child ++ [element])
                                                   | otherwise = Container2 cName [addElement c element toName | c <- child]
addElement x _ _ = x
