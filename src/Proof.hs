module Proof where

import Formula

type Proof = [Formula]

type Context = [Formula]

logicalAxioms :: Context
logicalAxioms = []

data Result = Correct
            | Incorrect Context Formula

instance Show Result where
  show Correct = "Correct"
  show (Incorrect phis phi) = "Incorrect step: \n" ++ show phis ++ "\nto\n" ++ show phi

instance Eq Result where
  (==) Correct Correct = True
  (==) (Incorrect _ _) (Incorrect _ _) = True
  (==) _ _ = False

-- NOTE: `proof` is in reverse order, i.e. `head proof` is the proven formula
checkProof :: Context -> Proof -> Result
checkProof axioms proof = if null proof then Correct else
  let phi = head proof
      phi_before = tail proof
  in
    if (phi `elem` logicalAxioms ||
    phi `elem` axioms ||
    any (\(phi1, phi2) -> phi1 == Imp phi2 phi) [(phi1, phi2) | phi1 <- phi_before, phi2 <- phi_before])
    then
      checkProof axioms phi_before
    else
      Incorrect phi_before phi

