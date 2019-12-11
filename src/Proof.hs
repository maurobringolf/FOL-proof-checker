module Proof where

import Formula

type Proof = [Formula]

type Axioms = [Formula]

logicalAxioms :: Axioms
logicalAxioms = []

checkProof :: Axioms -> Proof -> Bool
checkProof axioms proof = null proof ||
  let phi = head proof
      phi_before = tail proof
  in
    (phi `elem` logicalAxioms ||
    phi `elem` axioms ||
    any (\(phi1, phi2) -> phi1 == Imp phi2 phi) [(phi1, phi2) | phi1 <- phi_before, phi2 <- phi_before])
      &&
    checkProof axioms phi_before

