import qualified Data.Map as Map
import Expr (Expr(..), eval, simplify, differentiate, toString)

data Command
  = Push Double
  | PushVar String
  | Add
  | Sub
  | Mul
  | Div
  deriving (Show)

type Stack = [Double]
type Vars = Map.Map String Double
type Result = Either String Stack

runCommand :: Vars -> Stack -> Command -> Result
runCommand _ stack (Push x) = Right (x : stack)
runCommand vars stack (PushVar var) =
    case Map.lookup var vars of
        Just v  -> Right (v : stack)
        Nothing -> Left $ "Variable " ++ var ++ " not found"
runCommand _ (x:y:stack) Add = Right ((y + x) : stack)
runCommand _ _ Add = Left "Not enough operands for addition"
runCommand _ (x:y:stack) Sub = Right ((y - x) : stack)
runCommand _ _ Sub = Left "Not enough operands for subtraction"
runCommand _ (x:y:stack) Mul = Right ((y * x) : stack)
runCommand _ _ Mul = Left "Not enough operands for multiplication"
runCommand _ (x:y:stack) Div =
    if x /= 0
    then Right ((y / x) : stack)
    else Left "Division by zero"
runCommand _ _ Div = Left "Not enough operands for division"

runProgram :: Vars -> [Command] -> Stack -> Result
runProgram _ [] stack = Right stack
runProgram vars (cmd:cmds) stack =
    case runCommand vars stack cmd of
        Left err -> Left err
        Right newStack -> runProgram vars cmds newStack
        
parseCommand :: String -> Either String Command
parseCommand str = case words str of
    ["push", val] -> case reads val of
        [(n, "")] -> Right (Push n)
        _         -> Left $ "Invalid value: " ++ val
    ["push", var] -> Right (PushVar var)
    ["add"]       -> Right Add
    ["sub"]       -> Right Sub
    ["mul"]       -> Right Mul
    ["div"]       -> Right Div
    _             -> Left $ "Unknown command: " ++ str

parseProgram :: String -> Either String [Command]
parseProgram = mapM parseCommand . lines

exprToCommands :: Expr -> [Command]
exprToCommands (Const x) = [Push x]
exprToCommands (Var x) = [PushVar x]
exprToCommands (UnaryOp "-" e) = exprToCommands e ++ [Push 0, Sub]
exprToCommands (BinaryOp "+" e1 e2) = exprToCommands e1 ++ exprToCommands e2 ++ [Add]
exprToCommands (BinaryOp "-" e1 e2) = exprToCommands e1 ++ exprToCommands e2 ++ [Sub]
exprToCommands (BinaryOp "*" e1 e2) = exprToCommands e1 ++ exprToCommands e2 ++ [Mul]
exprToCommands (BinaryOp "/" e1 e2) = exprToCommands e1 ++ exprToCommands e2 ++ [Div]
