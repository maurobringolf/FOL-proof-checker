module Formula where

import Signature
import Term

import Data.List(intercalate)

data Formula = Rel Symbol [Term]
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             | Imp Formula Formula
             | EX Symbol Formula
             | FA Symbol Formula
  deriving (Eq)

infixr 5 ∧
(∧) = And

infixr 4 ∨
(∨) = Or

infixr 3 →
(→) = Imp

infixr 6 ≡
(≡) τ σ = Rel "=" [τ, σ]

c = Const
v = Var

instance Show Formula where
  show f = case f of
    Not f1    -> "¬" ++ show f1 
    And f1 f2 -> "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
    Or f1 f2  -> "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
    Imp f1 f2 -> "(" ++ show f1 ++ " → " ++ show f2 ++ ")"
    EX v f1   -> "(∃" ++ v ++ "." ++ show f1 ++ ")"
    FA v f1   -> "(∀" ++ v ++ "." ++ show f1 ++ ")"
    Rel op [l,r] -> show l ++ " " ++ op ++ " " ++ show r
    Rel op args -> op ++ "(" ++ Data.List.intercalate "," (map show args) ++ ")"

