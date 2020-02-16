module Theory.GT where

import Term
import Formula
import Context

axioms :: Context
axioms = [ -- GT_0 TODO
           -- GT_1 (Left-neutral element): ∀x e ∘ x = x
           Literal $ FA "x" (Rel "=" [FApp "∘" [Const "e", Var "x"], Var "x"])
           -- GT_2 (Left-inverse element): ∀x∃y y ∘ x = e
         , Literal $ FA "x" $ EX "y" $ Rel "=" [FApp "∘" [Var "y", Var "x"], Const "e"]
         ]

