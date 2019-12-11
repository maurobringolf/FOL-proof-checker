module Signature where

type Symbol = String

data Signature = Signature { constants :: [Symbol]
                 , functions :: [(Symbol, Int)]
                 , relations :: [(Symbol, Int)]
} deriving (Eq, Show)

sig_empty :: Signature
sig_empty = Signature { constants = []
                      , functions = []
                      , relations = []
}

