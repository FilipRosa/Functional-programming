data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Char
          | If BExpr Expr Expr deriving (Eq)

eval :: Expr -> Int
eval (Num x) = x
eval (Add l r) = (eval l) + (eval r)
eval (Sub l r) = (eval l) - (eval r)
eval (Mul l r) = (eval l) * (eval r)
eval (Div l r) = (eval l) `div` (eval r)
eval (If x l r) = if bEval x then eval l else eval r

showExpr :: Expr -> String
showExpr expr = showExpr' expr NoOp

data Operation = Hi | HiDiv | Lo | LoSub | NoOp deriving (Eq)

showExpr' :: Expr -> Operation -> String
showExpr' (Num x) _ = show x
showExpr' (Var x) _ = [x]
showExpr' (Add l r) op = let x = showExpr' l Lo ++ "+" ++ showExpr' r Lo
                         in if op == Hi || op == HiDiv || op == LoSub then "(" ++ x ++ ")" else x
showExpr' (Sub l r) op = let x = showExpr' l Lo ++ "-" ++ showExpr' r Lo
                         in if op == Hi || op == HiDiv || op == LoSub then "(" ++ x ++ ")" else x
showExpr' (Mul l r) op = let x = showExpr' l Hi ++ "*" ++ showExpr' r Hi
                         in if op == HiDiv then "(" ++ x ++ ")" else x
showExpr' (Div l r) op = let x = showExpr' l Hi ++ "/" ++ showExpr' r HiDiv
                         in if op == HiDiv then "(" ++ x ++ ")" else x
showExpr' (If x l r) _ = "if " ++ showBEexpr x ++ " then " ++ showExpr' l NoOp ++ " else " ++ showExpr' r NoOp 

instance (Show Expr) where
    show = showExpr

deriv :: Expr -> Char -> Expr
deriv (Num _) _ = (Num 0)
deriv (Var x) y | x == y = (Num 1)
                | otherwise = (Num 0)
deriv (Add l r) x = Add (deriv l x) (deriv r x)
deriv (Sub l r) x = Sub (deriv l x) (deriv r x)
deriv (Mul l r) x = Add (Mul (deriv l x) r) (Mul l (deriv r x))
deriv (Div l r) x = Div (Sub (Mul (deriv l x) r) (Mul l (deriv r x))) (Mul r r)

size :: Expr -> Int
size (Num _) = 0
size (Var _) = 0
size (Add l r) = 1 + size l + size r
size (Sub l r) = 1 + size l + size r
size (Mul l r) = 1 + size l + size r
size (Div l r) = 1 + size l + size r

data BExpr = Val Bool
        | And BExpr BExpr
        | Not BExpr 
        | Equal Expr Expr
        | Greater Expr Expr deriving (Eq)

bEval :: BExpr -> Bool
bEval (And l r) = (bEval l) && (bEval r)
bEval (Not x) = not (bEval x)
bEval (Equal l r) = eval l == eval r
bEval (Greater l r) = eval l > eval r

showBEexpr :: BExpr -> String
showBEexpr (Val True) = "True"
showBEexpr (Val False) = "False"
showBEexpr (Not b) = "!" ++ "(" ++ showBEexpr b ++ ")"
showBEexpr (Equal x y) = "(" ++ showExpr x ++ "==" ++ showExpr y ++ ")"
showBEexpr (Greater x y) = "(" ++ showExpr x ++ ">" ++ showExpr y ++ ")"
showBEexpr (And x y) = "(" ++ showBEexpr x ++ "/\\" ++ showBEexpr y ++ ")"
showBEexpr _ = "Unknown BExpr"

instance (Show BExpr) where
    show = showBEexpr