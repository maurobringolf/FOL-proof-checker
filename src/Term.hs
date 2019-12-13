module Term where

import Signature
import qualified Data.List(lookup, intercalate)

data Term = Const Symbol
          | Var Symbol
          | FApp Symbol [Term]
  deriving (Eq)

instance Show Term where
  show t = case t of
    Const c   -> c
    Var x     -> x
    FApp f ts -> f ++ "(" ++ Data.List.intercalate "," (map show ts) ++ ")"


getConstants :: Term -> [Symbol]
getConstants t = case t of
  Const c   -> [c]
  Var x     -> []
  FApp f ts -> concatMap getConstants ts

wf_term :: Signature -> Term -> Bool
wf_term sig t = case t of
  Const x -> x `elem` constants sig
  Var x -> True
  FApp f ts -> case Data.List.lookup f (functions sig) of
    Just n -> n == length ts && all (wf_term sig) ts
    Nothing -> False


