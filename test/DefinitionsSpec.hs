module DefinitionsSpec where

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

definitionTests :: IO ()
definitionTests = do

  defineConst <- readFile "test/proofs/correct/define-const.proof"

  hspec $ do
    describe "custom definitions" $ do
      it "Define 1 in PA" $ do
        let (_, ctxt, proof) = parse defineConst
        checkProof ctxt proof `shouldBe` Correct
        
