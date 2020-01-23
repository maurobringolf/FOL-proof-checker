module Proof where

import Signature
import Term
import Formula
import AstUtils
import Context

import qualified Theory.Logical(axioms)
import qualified Data.Set

type Proof = [Formula]


isInstanceOf :: Formula -> Axiom -> Bool
f `isInstanceOf` ax = case ax of
  Literal f' -> f' == f
  Schema f'  -> f' f





data Result = Correct
            | Incorrect Context Formula

instance Show Result where
  show Correct = "Correct"
  show (Incorrect phis phi) = "Incorrect. Cannot infer: \n" ++ show phi

instance Eq Result where
  (==) Correct Correct = True
  (==) (Incorrect _ _) (Incorrect _ _) = True
  (==) _ _ = False

byModusPonens :: Proof -> Formula -> Bool
byModusPonens phi_before phi = any (\(phi1, phi2) -> phi1 == Imp phi2 phi) [(phi1, phi2) | phi1 <- phi_before, phi2 <- phi_before]

byGeneralisation :: Context -> Proof -> Formula -> Bool
byGeneralisation nonLogicalAxioms phi_before f = case f of
  FA x f' -> f' `elem` phi_before && not (any (Data.Set.member x) (map (\a -> case a of
    Literal f'' -> freeF f''
    Schema _    -> Data.Set.empty
    ) nonLogicalAxioms))
  _       -> False


-- NOTE: `proof` is in reverse order, i.e. `head proof` is the proven formula
checkProof :: Context -> Proof -> Result
checkProof nonLogicalAxioms proof = if null proof then Correct else
  let phi = head proof
      phi_before = tail proof
  in
    if (any (isInstanceOf phi) (Theory.Logical.axioms ++ nonLogicalAxioms) ||
        byModusPonens phi_before phi ||
        byGeneralisation nonLogicalAxioms phi_before phi)
    then
      checkProof nonLogicalAxioms phi_before
    else
      Incorrect nonLogicalAxioms phi

