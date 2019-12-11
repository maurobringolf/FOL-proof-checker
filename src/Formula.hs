module Formula where

import Signature
import Term

data Formula = Eq Term Term
             | Rel Symbol [Term]
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imp Formula Formula
             | EX Symbol Formula
             | FA Symbol Formula
  deriving (Eq)

instance Show Formula where
  show f = case f of
    Eq t1 t2  -> "(" ++ show t1 ++ " = " ++ show t2 ++ ")"
    Not f1    -> "¬" ++ show f1 
    And f1 f2 -> "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
    Or f1 f2  -> "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
    Imp f1 f2 -> "(" ++ show f1 ++ " → " ++ show f2 ++ ")"
    EX v f1   -> "(∃" ++ v ++ "." ++ show f1 ++ ")"
    FA v f1   -> "(∀" ++ v ++ "." ++ show f1 ++ ")"

