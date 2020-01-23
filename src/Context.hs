module Context where

import Formula

data Axiom = Literal Formula | Schema (Formula -> Bool)
type Context = [Axiom]

