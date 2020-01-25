module Theory.PA where

import Context
import Term
import Formula
import AstUtils
import qualified Data.Set

axioms :: Context
axioms = [ -- PA_0: ¬∃x(s(x) = 0)
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
