module Theory.Logical where

import Term
import Formula
import Context
import AstUtils

axioms :: Context
axioms = map Schema [ l0
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

