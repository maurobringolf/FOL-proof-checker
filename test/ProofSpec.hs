module ProofSpec where

import Test.Hspec
import Test.QuickCheck

import TestUtils

import Signature
import Term
import Formula
import Proof

import Parser

proofTests :: IO ()
proofTests = do

  simpleMP <- readFile "test/proofs/correct/simple-modus-ponens.proof"
  justAnd  <- readFile "test/proofs/correct/just-and.proof"
  justOr  <- readFile "test/proofs/correct/just-or.proof"

  justEq   <- readFile "test/proofs/incorrect/just-equality.proof"
  fAppEq   <- readFile "test/proofs/incorrect/fapp-equality.proof"

  hspec $ do
    describe "checkProof" $ do
      it "simple modus ponens" $ do
        let (_, ctxt, proof) = parse simpleMP
        checkProof ctxt proof `shouldBe` Correct

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
        `shouldBe` Just (Eq (Var "x") (Var "y"))

      it "just eq" $ do
        let (_, ctxt, proof) = parse fAppEq
        case checkProof ctxt proof of
          Incorrect _ f -> Just f
          Correct -> Nothing
        `shouldBe` Just (Eq (FApp "f" [Const "0"]) (FApp "g" [Const "1"]))
      
      it "example 1.1" $
        
        property $ \phi ->
            checkProof []
            (reverse [ -- (φ → ((φ → φ) → φ)) → ((φ → (φ → φ)) → (φ → φ))
              Imp (Imp phi (Imp (Imp phi phi) phi)) (Imp (Imp phi (Imp phi phi)) (Imp phi phi))
            , -- φ -> ((φ -> φ) -> φ)
              Imp phi (Imp (Imp phi phi) phi)
            ,
              -- (φ -> (φ -> φ)) -> (φ -> φ)
              Imp (Imp phi (Imp phi phi)) (Imp phi phi)
            , -- φ -> (φ -> φ)
              Imp phi (Imp phi phi)
            , -- φ -> φ
              Imp phi phi
            ]) == Correct

        
