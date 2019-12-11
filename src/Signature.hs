module Signature where

type Symbol = String

data Signature = Signature { constants :: [Symbol]
                 , functions :: [(Symbol, Int)]
                 , relations :: [(Symbol, Int)]
}

sig_empty :: Signature
sig_empty = Signature { constants = []
                      , functions = []
                      , relations = []
}

