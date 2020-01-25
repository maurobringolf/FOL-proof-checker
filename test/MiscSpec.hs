module MiscSpec where

import Test.Hspec
import Test.QuickCheck

import TestUtils

import qualified Signature as Sig
import Term
import Formula
import Proof
import Context

import ParserSpec(parseTests)

miscTests :: IO ()
miscTests = do

  hspec $ do
    describe "misc tests" $ do
      it "`x` is well-formed" $ do
        wf_term Sig.empty (v "x") `shouldBe` True

      it "Any variable is well-formed" $
        property $ \s -> wf_term Sig.empty (Var s)

      it "Any constant is well-formed if it is in the signature" $
        property $ \s -> wf_term (Sig.empty {Sig.constants = [s]} ) (c s)

      it "φ |- φ" $
        property $ \phi -> Correct == checkProof [Literal phi] [phi] 

      it "A = B -> C = D, A = B |- C = D" $
        checkProof (map Literal [ c "A" ≡ c "B" → c "C" ≡ c "D"
                   , c "A" ≡ c "B"
                   ])
                   (reverse [
                     c "A" ≡ c "B" → c "C" ≡ c "D"
                   , c "A" ≡ c "B"
                   , c "C" ≡ c "D"
                   ]) `shouldBe` Correct

      it "|- t1 = t2" $
        property $ \t -> checkProof [] [t ≡ t] == Correct

      it "φ -> ψ, φ |- ψ" $
        property $ \phi psi -> checkProof (map Literal [Imp phi psi, phi]) (reverse [Imp phi psi, phi, psi]) == Correct


  parseTests

