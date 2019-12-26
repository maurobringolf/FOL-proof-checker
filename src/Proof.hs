module Proof where

import Signature
import Term
import Formula
import AstUtils

type Proof = [Formula]

data Axiom = Literal Formula | Schema (Formula -> Bool)

isInstanceOf :: Formula -> Axiom -> Bool
f `isInstanceOf` ax = case ax of
  Literal f' -> f' == f
  Schema f'  -> f' f

type Context = [Axiom]

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
                                                             && x1 `elem` freeF f4
                                                             && f2 == f4
                                                             && f1 == substF x1 (Const "0") f4
                                                             && f3 == substF x1 (FApp "s" [Var x1]) f4
              _ -> False)
          ]

l0 f = case f of
  Or f1 (Not f2) -> f1 == f2
  _ -> False

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
  Imp f1 (Imp f2 (And f3 f4)) -> f1 == f4 && f2 == f3
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

l10 f = case f of
  Imp (FA x f1) f2 -> case findTau x f1 f2 of
    Nothing -> f1 == f2
    Just t  -> substF x t f1 == f2
  _ -> False

l11 f = case f of
  Imp f1 (EX x f2) -> case findTau x f2 f1 of
    Nothing -> f1 == f2
    Just t  -> substF x t f2 == f1
  _ -> False

l12 f = case f of
  Imp (FA x1 (Imp f1 f2)) (Imp f3 (FA x2 f4)) -> x1 == x2 &&
                                                 f1 == f3 &&
                                                 f2 == f4 &&
                                                 not (x1 `elem` freeF f1)
  _ -> False

l13 f = case f of
  Imp (FA x1 (Imp f1 f2)) (Imp (EX x2 f3) f4)  -> x1 == x2 &&
                                                 f1 == f3 &&
                                                 f2 == f4 &&
                                                 not (x1 `elem` freeF f2)
  _ -> False

l14 f = case f of
  Rel "=" [t1, t2] -> t1 == t2
  _ -> False

l15 f = case f of
  Imp f' (Imp (Rel r1 args1) (Rel r2 args2)) ->
    case (do (taus, taus') <- lhsToEquals f'
             return $ r1 == r2 && args1 == taus && args2 == taus') of
      Just  b -> b
      Nothing -> False
  _ -> False

l16 f = case f of
  Imp f' (Rel "=" [FApp f1 args1, FApp f2 args2]) ->
    (case (do (taus, taus') <- lhsToEquals f'
              return $ f1 == f2 && args1 == taus && args2 == taus') of
        Just  b -> b
        Nothing -> False)
  _ -> False


lhsToEquals :: Formula -> Maybe ([Term], [Term])
lhsToEquals f = case f of
  Rel "=" [tau, tau'] -> Just ([tau], [tau'])
  And rest (Rel "=" [tau, tau']) -> do (taus, taus') <- lhsToEquals rest
                                       return (taus ++ [tau], taus' ++ [tau'])
  And (Rel "=" [tau, tau']) rest -> do (taus, taus') <- lhsToEquals rest
                                       return (tau:taus, tau':taus')
  _ -> Nothing

logicalAxioms :: Context
logicalAxioms = map Schema [ l0
                           , l1
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
                           , l16
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

byGeneralisation :: Proof -> Formula -> Bool
byGeneralisation phi_before f = case f of
  FA x f' -> f' `elem` phi_before
  _       -> False


-- NOTE: `proof` is in reverse order, i.e. `head proof` is the proven formula
checkProof :: Context -> Proof -> Result
checkProof nonLogicalAxioms proof = if null proof then Correct else
  let phi = head proof
      phi_before = tail proof
  in
    if (any (isInstanceOf phi) (logicalAxioms ++ nonLogicalAxioms) ||
        byModusPonens phi_before phi ||
        byGeneralisation phi_before phi)
    then
      checkProof nonLogicalAxioms phi_before
    else
      Incorrect nonLogicalAxioms phi

