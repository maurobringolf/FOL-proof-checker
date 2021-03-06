module ProofSpec where

import Test.Hspec
import Test.QuickCheck

import TestUtils

import Signature
import Term
import Formula
import Proof
import AstUtils

import Parser

import qualified Data.Set as Set
import Data.Maybe(fromJust)

proofTests :: IO ()
proofTests = do

  simpleMP <- readFile "test/proofs/correct/simple-modus-ponens.proof"
  generalisation <- readFile "test/proofs/correct/generalisation.proof"
  eq_trans <- readFile "test/proofs/correct/equality-transitive.proof"
  incorrect_generalisation <- readFile "test/proofs/incorrect/generalisation.proof"
  justAnd  <- readFile "test/proofs/correct/just-and.proof"
  justOr  <- readFile "test/proofs/correct/just-or.proof"
  example_1_2_broken  <- readFile "test/proofs/incorrect/example-1.2.proof"
  example_1_1  <- readFile "test/proofs/correct/example-1.1.proof"
  example_1_3  <- readFile "test/proofs/correct/example-1.3.proof"
  example_1_2_fixed  <- readFile "test/proofs/correct/example-1.2.proof"
  example_1_4  <- readFile "test/proofs/correct/example-1.4.proof"
  l0   <- readFile "test/proofs/correct/L0.proof"
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

  hspec $ do
    describe "checkProof" $ do
      it "simple modus ponens" $ do
        let (_, ctxt, proof) = parse simpleMP
        checkProof ctxt proof `shouldBe` Correct

      it "generalisation" $ do
        let (_, ctxt, proof) = parse generalisation
        checkProof ctxt proof `shouldBe` Correct

      it "equality transitive" $ do
        let (_, ctxt, proof) = parse eq_trans
        checkProof ctxt proof `shouldBe` Correct

      it "incorrect generalisation" $
        let (_, ctxt, proof) = (parse incorrect_generalisation) in
        (case (checkProof ctxt proof) of
          Correct -> False
          Incorrect _ _ -> True
        ) `shouldBe` True

      it "just and" $ do
        let (_, ctxt, proof) = parse justAnd
        checkProof ctxt proof `shouldBe` Correct

      it "just or" $ do
        let (_, ctxt, proof) = parse justOr
        checkProof ctxt proof `shouldBe` Correct

      it "just eq" $ do
        let (_, ctxt, proof) = parse justEq
        case checkProof ctxt proof of
          Incorrect _ f -> Just f
          Correct -> Nothing
        `shouldBe` Just (Var "x" ≡ Var "y")

      it "just eq" $ do
        let (_, ctxt, proof) = parse fAppEq
        case checkProof ctxt proof of
          Incorrect _ f -> Just f
          Correct -> Nothing
        `shouldBe` Just (FApp "f" [Const "0"] ≡ FApp "g" [Const "1"])

      -- Check all the axioms
      it "L0" $ do
        let (_, ctxt, proof) = parse l0
        checkProof ctxt proof `shouldBe` Correct

      it "L0_incorrect" $ do
        let (_, ctxt, proof) = parse l0_incorrect
        (case (checkProof ctxt proof) of
          Correct -> False
          Incorrect _ _ -> True
          ) `shouldBe` True

      it "L1" $ do
        let (_, ctxt, proof) = parse l1
        checkProof ctxt proof `shouldBe` Correct

      it "L2" $ do
        let (_, ctxt, proof) = parse l2
        checkProof ctxt proof `shouldBe` Correct

      it "L3" $ do
        let (_, ctxt, proof) = parse l3
        checkProof ctxt proof `shouldBe` Correct

      it "L5" $ do
        let (_, ctxt, proof) = parse l5
        checkProof ctxt proof `shouldBe` Correct

      it "L8" $ do
        let (_, ctxt, proof) = parse l8
        checkProof ctxt proof `shouldBe` Correct

      it "L9" $ do
        let (_, ctxt, proof) = parse l9
        checkProof ctxt proof `shouldBe` Correct

      it "L15" $ do
        let (_, ctxt, proof) = parse l15
        checkProof ctxt proof `shouldBe` Correct

      it "L16" $ do
        let (_, ctxt, proof) = parse l16
        checkProof ctxt proof `shouldBe` Correct


      it "subst 1" $ do
        property $ \t -> substF "x" t (Rel "=" [Var "x", Var "y"]) == Rel "=" [t, Var "y"]

      it "subst 2" $ do
        property $ \t -> substF "x" t (FA "x" (Rel "=" [Var "x", Var "y"])) == (FA "x" (Rel "=" [Var "x", Var "y"])) 

      it "subst 3" $ do
        property $ \t -> substF "x" t (FA "z" (Rel "=" [Var "x", Var "y"])) == (FA "z" (Rel "=" [t, Var "y"])) 

      it "findTau 1" $ do
        findTau "x" (v "x" ≡ v "x") (c "1" ≡ c "1") `shouldBe` Just (c "1")
        findTau "x" (FA "z" (v "x" ≡ v "x")) (c "1" ≡ c "1") `shouldBe` Nothing
        findTau "x" (FA "z" (v "x" ≡ v "x")) (FA "z" (c "1" ≡ c "1")) `shouldBe` Just (c "1")
        findTau "x" (FA "a" (v "x" ≡ v "x")) (FA "z" (c "1" ≡ c "1")) `shouldBe` Nothing
        findTau "x" (v "x" ≡ v "x" ∧ v "x" ≡ v "x") (c "1" ≡ c "1" ∧ c "1" ≡ c "1") `shouldBe` Just (Const "1")

      it "findTau 2" $ do
        property $ \t f -> not (null (freeF f)) ==> let x = Data.Maybe.fromJust (Set.lookupGT "" (freeF f)) in
          findTau x f (substF x t f) == Just t

      it "L10" $ do
        let (_, ctxt, proof) = parse l10
        checkProof ctxt proof `shouldBe` Correct

      it "example 1.1" $
        let (_, ctxt, proof) = (parse example_1_1) in
        checkProof ctxt proof `shouldBe` Correct

      it "example 1.2 broken" $
        let (_, ctxt, proof) = (parse example_1_2_broken) in
        (case (checkProof ctxt proof) of
          Correct -> False
          Incorrect _ _ -> True
        ) `shouldBe` True

      it "example 1.2 fixed" $
        let (_, ctxt, proof) = (parse example_1_2_fixed) in
        checkProof ctxt proof `shouldBe` Correct

      it "example 1.3" $
        let (_, ctxt, proof) = (parse example_1_3) in
        checkProof ctxt proof `shouldBe` Correct

      it "example 1.4" $
        let (_, ctxt, proof) = parse example_1_4 in
        checkProof ctxt proof `shouldBe` Correct

