import Test.Hspec
import Test.QuickCheck

import Signature
import Term
import Formula
import Proof

import Data.Char(isAsciiLower)

import ProofSpec(proofTests)
import ParserSpec(parseTests)
import MiscSpec(miscTests)

main :: IO ()
main = do parseTests
          miscTests
          proofTests
          
