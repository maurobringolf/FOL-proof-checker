module Theory.ZF where

import Context
import Term
import Formula

axioms = [ -- Z0: Axiom of the empty set
           Literal $ EX "x" $ FA "z" $ Not $ Rel "e" [Var "z",Var "x"]
           -- Z1: Axiom of extensionality
         , Literal $ FA "x" $ FA "y" $ Imp
             (FA "z" $ And
               (Imp (Rel "e" [Var "z", Var "x"]) (Rel "e" [Var "z", Var "y"]))
               (Imp (Rel "e" [Var "z", Var "y"]) (Rel "e" [Var "z", Var "x"]))
             )
             (Rel "=" [Var "x", Var "y"])
         ]
