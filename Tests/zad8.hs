data Company = Name String
             | Employees Int
             | OwnerOf [Company] deriving (Show)

name' :: Company
name' = Name "Apple"

name'' :: Company
name'' = Name "Meta"

emplo' :: Company
emplo' = Employees 1000

emplo'' :: Company
emplo'' = Employees 5000

owner :: Company
owner = OwnerOf [name', emplo']

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

printPath :: Component -> String -> Maybe String
printPath (TextBox n _) target | n == target = Just n
                               | otherwise = Nothing

printPath (Button n _) target | n == target = Just n
                              | otherwise = Nothing

printPath (Container n child) target | n == target = Just n
                                     | otherwise = case findPathInChildren child target of 
                                            Nothing -> Nothing
                                            Just path -> Just (n ++ " -> " ++ path)

findPathInChildren [] _ = Nothing
findPathInChildren (c:cs) target = case printPath c target of
                                        Just path -> Just path
                                        Nothing -> findPathInChildren cs target

removeAtIndex children index | index < 0 || index >= length children = children
                             | otherwise = let (before, _:after) = splitAt index children in before ++ after

removeComponentFromContainerAtIndex :: Component -> String -> Int -> Component
removeComponentFromContainerAtIndex (Container n children) target index | n == target = Container n (removeAtIndex children index)
                                                                        | otherwise = Container n (map (\child -> removeComponentFromContainerAtIndex child target index) children)
removeComponentFromContainerAtIndex component _ _ = component                                                        