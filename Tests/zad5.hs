data Element = Text String
            | Button1 String String
            | Panel [Element] deriving (Show)

text' :: Element
text' = Text "Ahoj"

button :: Element
button = Button1 "btn1" "[text']"

panel :: Element
panel = Panel [button, text']

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

listAllButtons :: Component -> [Component]
listAllButtons (TextBox _ _) = []
listAllButtons (Button n v) = [Button n v]
listAllButtons (Container _ child) = concatMap listAllButtons child

removeAllButtons :: Component -> Component
removeAllButtons (Button _ _) = Container "" []
removeAllButtons (Container n child) = Container n (map removeAllButtons child) 
removeAllButtons x = x