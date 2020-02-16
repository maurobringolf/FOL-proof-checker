module Z3ProofSpec where

import Test.Hspec
import Test.QuickCheck hiding (Result)

import TestUtils

import Signature
import Term
import Formula
import Z3Proof
import AstUtils

import Parser

import qualified Data.Set as Set
import Data.Maybe(fromJust)

run :: String -> IO Result
run filename = do
  text <- readFile filename
  let (sig,ctxt,proof) = parse text
  checkProofWithZ3 sig ctxt proof
z3proofTests :: IO ()
z3proofTests = do
  simpleMP <- run "test/proofs/correct/simple-modus-ponens.proof"
  example_1_2 <- run "test/proofs/correct/example-1.2.proof"
  eq_trans <- run "test/proofs/correct/equality-transitive.proof"
  l0   <- run "test/proofs/correct/L0.proof"
  l1   <- run "test/proofs/correct/L1.proof"
  l2   <- run "test/proofs/correct/L2.proof"
  zfAxioms   <- run "test/proofs/correct/zf-axioms.proof"

  {-
  generalisation <- readFile "test/proofs/correct/generalisation.proof"
  incorrect_generalisation <- readFile "test/proofs/incorrect/generalisation.proof"
  justAnd  <- readFile "test/proofs/correct/just-and.proof"
  justOr  <- readFile "test/proofs/correct/just-or.proof"
  example_1_2_broken  <- readFile "test/proofs/incorrect/example-1.2.proof"
  example_1_1  <- readFile "test/proofs/correct/example-1.1.proof"
  example_1_3  <- readFile "test/proofs/correct/example-1.3.proof"
  example_1_4  <- readFile "test/proofs/correct/example-1.4.proof"
  l0_incorrect   <- readFile "test/proofs/incorrect/L0.proof"
  l1   <- readFile "test/proofs/correct/L1.proof"
  l2   <- readFile "test/proofs/correct/L2.proof"
  l3   <- readFile "test/proofs/correct/L3.proof"
  l5   <- readFile "test/proofs/correct/L5.proof"
  l8   <- readFile "test/proofs/correct/L8.proof"
  l9   <- readFile "test/proofs/correct/L9.proof"
  l10   <- readFile "test/proofs/correct/L10.proof"
  l15   <- readFile "test/proofs/correct/L15.proof"
  l16   <- readFile "test/proofs/correct/L16.proof"

  justEq   <- readFile "test/proofs/incorrect/just-equality.proof"
  fAppEq   <- readFile "test/proofs/incorrect/fapp-equality.proof"
  

  gt_right_neutral <- readFile "test/proofs/correct/gt-right-neutral.proof"
  gt_right_inverse <- readFile "test/proofs/correct/gt-right-inverse.proof"
  gt_eee <- readFile "test/proofs/correct/gt-e-e-e.proof"
  -}

  hspec $ do
    describe "checkProofWithZ3" $ do

      it "zfAxioms" $ do
        zfAxioms `shouldBe` Correct

      it "l0" $ do
        l0 `shouldBe` Correct

      it "l1" $ do
        l1 `shouldBe` Correct

      it "l2" $ do
        l2 `shouldBe` Correct

      it "simple modus ponens" $ do
        simpleMP `shouldBe` Correct

      it "example 1.2" $ do
        example_1_2 `shouldBe` Correct
        
      it "eq_trans" $ do
        eq_trans `shouldBe` Correct
