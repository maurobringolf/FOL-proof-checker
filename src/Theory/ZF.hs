module Theory.ZF where

import Context
import Term
import Formula

axioms = [ -- Z0: Axiom of the empty set
           Literal $ EX "x" $ FA "z" $ Not $ Rel "∈" [Var "z",Var "x"]
           -- Z1: Axiom of extensionality
         , Literal $ FA "x" $ FA "y" $ Imp
             (FA "z" $ And
               (Imp (Rel "∈" [Var "z", Var "x"]) (Rel "∈" [Var "z", Var "y"]))
               (Imp (Rel "∈" [Var "z", Var "y"]) (Rel "∈" [Var "z", Var "x"]))
             )
             (Rel "=" [Var "x", Var "y"])
         ]
