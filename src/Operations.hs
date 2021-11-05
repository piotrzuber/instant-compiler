module Operations where

data ArithmeticOp = AddOp | SubOp | MulOp | DivOp deriving Eq

data OpCommutativity = CommOp | NonCommOp deriving Eq

getCommutativity :: ArithmeticOp -> OpCommutativity
getCommutativity op 
    | op == AddOp || op == MulOp = CommOp
    | otherwise = NonCommOp