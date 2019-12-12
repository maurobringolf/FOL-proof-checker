module Signature where

type Symbol = String

data Signature = Signature { constants :: [Symbol]
                           , functions :: [(Symbol, Int)]
                           , binary_functions :: [Symbol]
                           , relations :: [(Symbol, Int)]
} deriving (Eq, Show)

sig_empty :: Signature
sig_empty = Signature { constants = []
                      , functions = []
                      , binary_functions = []
                      , relations = []
}

sig_PA :: Signature
sig_PA = Signature { constants = [ "0" ]
                   , functions = [ ("s", 1), ("(+)", 2) ]
                   , binary_functions = ["+"]
                   , relations = []
}
