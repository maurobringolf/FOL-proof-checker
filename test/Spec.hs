import Test.Hspec
import Test.QuickCheck

import Signature
import Term
import Formula
import Proof

import Data.Char(isAsciiLower)

import ProofSpec(proofTests)
import Z3ProofSpec(z3proofTests)
import ParserSpec(parseTests)
import MiscSpec(miscTests)
import DefinitionsSpec(definitionTests)

main :: IO ()
main = do parseTests
          miscTests
          proofTests
          z3proofTests
          definitionTests 
          

