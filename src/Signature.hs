module Signature where

type Symbol = String

data Signature = Signature { constants :: [Symbol]
                           , functions :: [(Symbol, Int)]
                           , binary_functions :: [Symbol]
                           , relations :: [(Symbol, Int)]
} deriving (Eq, Show)

empty :: Signature
empty = Signature { constants = []
                  , functions = []
                  , binary_functions = []
                  , relations = []
}

pa :: Signature
pa = Signature { constants = [ "0" ]
               , functions = [ ("s", 1), ("(+)", 2) ]
               , binary_functions = [ "+", "*" ]
               , relations = []
}

gt :: Signature
gt = Signature { constants = [ "e" ]
               , functions = []
               , binary_functions = [ "∘" ]
               , relations = []
}

st :: Signature
st = Signature { constants = []
               , functions = []
               , binary_functions = []
               , relations = [ ("∈", 2) ]
}
