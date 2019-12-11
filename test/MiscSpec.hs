module MiscSpec where

import Test.Hspec
import Test.QuickCheck

import TestUtils

import Signature
import Term
import Formula
import Proof

import ParserSpec(parseTests)

miscTests :: IO ()
miscTests = do

  hspec $ do
    describe "misc tests" $ do
      it "`x` is well-formed" $ do
        wf_term sig_empty (Var "x") `shouldBe` True

      it "Any variable is well-formed" $
        property $ \s -> wf_term sig_empty (Var s)

      it "Any constant is well-formed if it is in the signature" $
        property $ \s -> wf_term (sig_empty {constants = [s]} ) (Const s)

      it "φ |- φ" $
        property $ \phi -> Correct == checkProof [phi] [phi] 

      it "A = B -> C = D, A = B |- C = D" $
        checkProof [ Imp (Eq (Const "A") (Const "B")) (Eq (Const "C") (Const "D")) -- A = B -> C = D
                   , Eq (Const "A") (Const "B")                                    -- A = B
                   ]
                   (reverse [ Imp (Eq (Const "A") (Const "B")) (Eq (Const "C") (Const "D")) -- A = B -> C = D
                   , Eq (Const "A") (Const "B")                                    -- A = B
                   , Eq (Const "C") (Const "D")                                    -- C = D
                   ]) `shouldBe` Correct

      it "|- t1 = t2" $
        property $ \t1 -> checkProof [] [Eq t1 t1] == Correct

      it "φ -> ψ, φ |- ψ" $
        property $ \phi psi -> checkProof [Imp phi psi, phi] (reverse [Imp phi psi, phi, psi]) == Correct


  parseTests

