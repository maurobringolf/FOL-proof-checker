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

ctxt_PA :: Context
ctxt_PA = [ -- PA_0: ¬∃x(s(x) = 0)
            Literal $ Not (EX "x" (Rel "=" [FApp "s" [Var "x"], (Const "0")]))
          , -- PA_1: ∀x∀y(s(x) = s(y) -> x = y)
            Literal $ FA "x" (FA "y" (Imp (Rel "=" [FApp "s" [Var "x"], FApp "s" [Var "y"]]) (Rel "=" [Var "x", Var "y"])))
          , -- PA_2: ∀x(x + 0 = x)
            Literal $ FA "x" (Rel "=" [FApp "+" [Var "x", Const "0"], Var "x"])
          , -- PA_3: ∀x∀y(x + s(y) = s(x + y))
            Literal $ FA "x" (FA "y" (Rel "="
              [FApp "+" [Var "x", FApp "s" [Var "y"]]
              ,FApp "s" [FApp "+" [Var "x", Var "y"]]]
              ))
          , -- PA_4: ∀x(x * 0 = 0)
            Literal $ FA "x" (Rel "="
              [ FApp "*" [Var "x", Const "0"]
              , Const "0"
              ])
          , -- PA_5: ∀x∀y(x * s(y) = (x*y) + x)
            Literal $ FA "x" (FA "y" (Rel "="
              [ FApp "*" [Var "x", FApp "s" [Var "y"]]
              , FApp "+" [FApp "*" [Var "x", Var "y"], Var "x"]
              ]))
          , -- PA_6: If x ∈ free(φ) then (φ(0) ∧ ∀x(φ(x) -> φ(s(x)))) -> ∀xφ(x)
            Schema (\f -> case f of
              Imp (And f1 (FA x1 (Imp f2 f3))) (FA x2 f4) -> x1 == x2
                                                             && Data.Set.singleton x1 == freeF f4
                                                             && f2 == f4
                                                             && f1 == substF x1 (Const "0") f4
                                                             && f3 == substF x1 (FApp "s" [Var x1]) f4
              _ -> False)
          ]




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

