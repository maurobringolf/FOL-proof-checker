
module Proof where

import Signature
import Term
import Formula
import AstUtils


type Proof = [Formula]

type Context = [Formula]

l1 f = case f of
  Imp f1 (Imp f2 f3) -> f1 == f3 
  _ -> False

l2 f = case f of
  Imp
    (Imp f1 (Imp f2 f3))
    (Imp (Imp f4 f5) (Imp f6 f7)) -> f1 == f4 && f2 == f5 && f1 == f6 && f3 == f7
  _ -> False

l3 f = case f of
  Imp (And f1 f2) f3 -> f1 == f3 
  _ -> False

l4 f = case f of
  Imp (And f1 f2) f3 -> f2 == f3 
  _ -> False

l5 f = case f of
  Imp f1 (Imp f2 (And f3 f4)) -> f1 == f3 && f4 == f2
  _ -> False

l6 f = case f of
  Imp f1 (Or f2 f3) -> f1 == f2
  _ -> False
  
l7 f = case f of
  Imp f1 (Or f2 f3) -> f1 == f3
  _ -> False

l8 f = case f of
  Imp (Imp f1 f2) (Imp
    (Imp f3 f4)
    (Imp (Or f5 f6) f7)) -> f1 == f5 && f2 == f4 && f3 == f6 && f2 == f7
  _ -> False

l9 f = case f of
  Imp (Not f1) (Imp f2 f3) -> f1 == f2
  _ -> False

-- TODO
l10 f = case f of
  Imp (FA x f1) f2 -> let
    tau = findTau x f1 f2
    in
      True
  _ -> False

-- TODO
l11 f = case f of
  _ -> False

-- TODO
l12 f = case f of
  _ -> False

-- TODO
l13 f = case f of
  _ -> False

l14 f = case f of
  Eq t1 t2 -> t1 == t2
  _ -> False

-- TODO
l15 f = case f of
  _ -> False

logicalAxiom :: Formula -> Bool
logicalAxiom f = foldr (\l b -> b || l f) False [ l1
                                                , l2
                                                , l3
                                                , l4
                                                , l5
                                                , l6
                                                , l7
                                                , l8
                                                , l9
                                                , l10
                                                , l11
                                                , l12
                                                , l13
                                                , l14
                                                , l15
                                                ]

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
    if (logicalAxiom phi ||
    phi `elem` axioms ||
    any (\(phi1, phi2) -> phi1 == Imp phi2 phi) [(phi1, phi2) | phi1 <- phi_before, phi2 <- phi_before])
    then
      checkProof axioms phi_before
    else
      Incorrect phi_before phi

