module Expr
  ( Expr(..)
  , eval
  , simplify
  , differentiate
  , toString
  ) where

import qualified Data.Map as Map

data Expr
  = Const Double
  | Var String
  | UnaryOp String Expr
  | BinaryOp String Expr Expr
  deriving (Eq)

toString :: Expr -> String
toString (Const x) = show x
toString (Var x) = x
toString (UnaryOp op e) = op ++ "(" ++ toString e ++ ")"
toString (BinaryOp op e1 e2) = "(" ++ toString e1 ++ " " ++ op ++ " " ++ toString e2 ++ ")"

eval :: Map.Map String Double -> Expr -> Double
eval _ (Const x) = x
eval vars (Var x) = case Map.lookup x vars of
                      Just v  -> v
                      Nothing -> error $ "Variable " ++ x ++ " not found"
eval vars (UnaryOp op e) =
  let v = eval vars e
  in case op of
       "-"    -> -v
       "cos"  -> cos v
       "sin"  -> sin v
       "sqrt" -> sqrt v
       _      -> error $ "Unknown unary operator: " ++ op
eval vars (BinaryOp op e1 e2) =
  let v1 = eval vars e1
      v2 = eval vars e2
  in case op of
       "+"  -> v1 + v2
       "-"  -> v1 - v2
       "*"  -> v1 * v2
       "/"  -> v1 / v2
       "max" -> max v1 v2
       "min" -> min v1 v2
       _    -> error $ "Unknown binary operator: " ++ op

simplify :: Expr -> Expr
simplify (BinaryOp "+" (Const 0) e) = simplify e
simplify (BinaryOp "+" e (Const 0)) = simplify e
simplify (BinaryOp "*" (Const 1) e) = simplify e
simplify (BinaryOp "*" e (Const 1)) = simplify e
simplify (BinaryOp "*" (Const 0) _) = Const 0
simplify (BinaryOp "*" _ (Const 0)) = Const 0
simplify (BinaryOp "-" e1 e2) 
  | e1 == e2  = Const 0
simplify (BinaryOp op e1 e2) = BinaryOp op (simplify e1) (simplify e2)
simplify (UnaryOp op e) = UnaryOp op (simplify e)
simplify e = e

differentiate :: String -> Expr -> Expr
differentiate _ (Const _) = Const 0
differentiate var (Var x) 
  | var == x  = Const 1
  | otherwise = Const 0
differentiate var (UnaryOp op e) = 
  case op of
    "-" -> UnaryOp "-" (differentiate var e)
    "cos" -> UnaryOp "-" (BinaryOp "*" (UnaryOp "sin" e) (differentiate var e))
    "sin" -> BinaryOp "*" (UnaryOp "cos" e) (differentiate var e)
    "sqrt" -> BinaryOp "/" (differentiate var e) (BinaryOp "*" (Const 2) (UnaryOp "sqrt" e))
    _ -> error $ "Unknown unary operator: " ++ op
differentiate var (BinaryOp op e1 e2) = 
  case op of
    "+" -> BinaryOp "+" (differentiate var e1) (differentiate var e2)
    "-" -> BinaryOp "-" (differentiate var e1) (differentiate var e2)
    "*" -> BinaryOp "+" (BinaryOp "*" (differentiate var e1) e2) (BinaryOp "*" e1 (differentiate var e2))
    "/" -> BinaryOp "/" (BinaryOp "-" (BinaryOp "*" (differentiate var e1) e2) (BinaryOp "*" e1 (differentiate var e2))) (BinaryOp "*" e2 e2)
    _ -> error $ "Unknown binary operator: " ++ op
