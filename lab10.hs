data Tree a = Leaf a
            | Branch a (Tree a) (Tree a) deriving (Show)

testTree1 :: Tree Int
testTree1 = Branch 12 (Branch 23 (Leaf 34) (Leaf 45)) (Leaf 55)

testTree2 :: Tree Char
testTree2 = Branch 'a' (Branch 'b' (Leaf 'c') (Leaf 'd')) (Leaf 'e')

sum' :: Tree Int -> Int
sum' (Leaf x) = x
sum' (Branch x l r) = sum' l + x + sum' r

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Branch x l r) = toList l ++ [x] ++ toList r

maxTree :: Ord a => Tree a -> a
maxTree (Leaf x) = x
maxTree (Branch x l r) = maximum [x, maxTree l, maxTree r]

depthTree :: Tree a -> Int
depthTree (Leaf x) = 1
depthTree (Branch x l r) = max (depthTree l) (depthTree r) + 1

getGreaterElements :: Ord a => Tree a -> a -> [a]
getGreaterElements (Leaf x) y | x > y = [x]
                              | otherwise = []
getGreaterElements (Branch x l r) y | x > y = [x] ++ getGreaterElements l y ++ getGreaterElements r y
                                    | otherwise = getGreaterElements l y ++ getGreaterElements r y

toString :: Show a => Tree a -> String
toString (Leaf x) = show x
toString (Branch x l r) = show x ++ "(" ++ (toString l) ++ "," ++ (toString r) ++ ")"

fromString :: Read a => String -> Tree a
fromString inp = fst (fromString' inp) where
    fromString' :: Read a => String -> (Tree a, String)
    fromString' inp = 
                    let
                        before = takeWhile (\x -> x /= '(' && x/= ',' && x/= ')') inp
                        after = dropWhile (\x -> x /= '(' && x/= ',' && x/= ')') inp
                        value = read before
                    in
                        if null after || head after /= '(' then (Leaf value, after) else
                            let
                                (l, after') = fromString' (tail after)
                                (r, after'') = fromString' (tail after')
                            in
                                (Branch value l r, tail after'')

leafCount :: Tree a -> Int
leafCount (Leaf _) = 1
leafCount (Branch _ l r) = leafCount l + leafCount r

branchCount :: Tree a -> Int
branchCount (Leaf _) = 0
branchCount (Branch _ l r) = 1 + branchCount l + branchCount r

contains :: Eq a => Tree a -> a -> Bool
contains (Leaf x) y = x == y
contains (Branch x l r) y | x == y = True
                          | otherwise = contains l y || contains r y

greatherThan :: Ord a => Tree a -> a -> Int
greatherThan (Leaf x) y | x > y = 1
                        | otherwise = 0
greatherThan (Branch x l r) y | x > y = 1 + greatherThan l y + greatherThan r y
                              | otherwise = greatherThan l y + greatherThan r y

data Tree2 a = Null | Branch2 a (Tree2 a) (Tree2 a) deriving (Show)

testTree3 :: Tree2 Int
testTree3 = Branch2 12 (Branch2 23 (Branch2 34 Null Null) (Branch2 45 Null Null)) (Branch2 55 Null Null)

leafCount' :: Tree2 a -> Int
leafCount' Null = 0
leafCount' (Branch2 _ l r) = leafCount' l + leafCount' r

branchCount' :: Tree2 a -> Int
branchCount' Null = 0
branchCount' (Branch2 _ l r) = 1 + branchCount' l + branchCount' r

contains' :: Eq a => Tree2 a -> a -> Bool
contains' Null _ = False
contains' (Branch2 x l r) y | x == y = True
                          | otherwise = contains' l y || contains' r y

greatherThan' :: Ord a => Tree2 a -> a -> Int
greatherThan' Null _ = 0
greatherThan' (Branch2 x l r) y | x > y = 1 + greatherThan' l y + greatherThan' r y
                               | otherwise = greatherThan' l y + greatherThan' r y

module Stack(Stack, emptyS, push, pop, top, isEmpty) where
    data Stack a = Stack [a] deriving (Show)

    emptyS :: Stack a
    emptyS = Stack []

    push :: a -> Stack a -> Stack a
    push x (Stack y) = Stack (x:y)

    pop :: Stack a -> Stack a
    pop (Stack (_:xs)) = Stack xs

    top :: Stack a -> a
    top (Stack (x:_)) = x

    isEmpty :: Stack a -> Bool
    isEmpty (Stack []) = True
    isEmpty _ = False

module Queue(Queue, emptyQ, isEmptyQ, addQ, remQ) where
    data Queue a = Qu [a] deriving (Show)

    emptyQ :: Queue a
    emptyQ = Qu []

    isEmptyQ :: Queue a -> Bool
    isEmptyQ (Qu q) = null q

    addQ :: a -> Queue a -> Queue a
    addQ x (Qu xs) = Qu (xs++[x])

    remQ :: Queue a -> (a, Queue a)
    remQ q@(Qu xs) | not (isEmpty q) = (head xs, Qu (tail xs))
                   | otherwise = error "remQ"