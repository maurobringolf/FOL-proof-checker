module ParserSpec where

import Test.Hspec
import Test.QuickCheck

import Signature
import Term
import Formula
import Proof
import Parser
import AstUtils
import Context

import TestUtils

import Data.Char(isAsciiLower)
import Data.List(intercalate)

instance Show Axiom where
  show (Literal f) = show f
  show (Schema _)  = "Cannot show Axiomschema"

instance Eq Axiom where
  (Literal f1) == (Literal f2) = f1 == f2
  _ == _ = False

parseTests :: IO ()
parseTests = do
  simpleMP <- readFile "test/proofs/correct/simple-modus-ponens.proof"
  justEq   <- readFile "test/proofs/incorrect/just-equality.proof"
  fAppEq   <- readFile "test/proofs/incorrect/fapp-equality.proof"
  justAnd  <- readFile "test/proofs/correct/just-and.proof"
  justOr  <- readFile "test/proofs/correct/just-or.proof"
  l9  <- readFile "test/proofs/correct/L9.proof"

  hspec $ do
    describe "Parser tests" $ do
      it ("parse simple-modus-ponens.proof") $ do
        parse simpleMP `shouldBe` ( sig_empty { constants = ["A", "B", "C", "D"]}
                                  , map Literal [c "A" ≡ c "B" → c "C" ≡ c "D"
                                    , c "A" ≡ c "B"
                                    ]
                                  , [ c "C" ≡ c "D"
                                    , c "A" ≡ c "B"
                                    , c "A" ≡ c "B" → c "C" ≡ c "D"
                                    ])

      it ("parse just-equality.proof") $ do
        parse justEq `shouldBe` (sig_empty, [], [v "x" ≡ v "y"])
        
      it ("parse fapp-equality.proof") $ do
        parse fAppEq `shouldBe` (sig_empty { constants = ["0", "1"] }, [], [FApp "f" [Const "0"] ≡ FApp "g" [Const "1"]])

      it ("parse L9.proof") $ do
        parse l9 `shouldBe` (sig_empty, [], reverse [
          Not (v "x" ≡ v "x") → v "x" ≡ v "x" → v "x" ≡ v "y" 
          , (Not (Rel "φ" [])) → (Rel "φ" []) → (Rel "ψ" [])
          ])

      it ("parse just-and.proof") $ do
        parse justAnd `shouldBe` (sig_empty { constants = [ "1" ]}, [],
          [ c "1" ≡ c "1" ∧ c "1" ≡ c "1" 
          , c "1" ≡ c "1" → c "1" ≡ c "1" ∧ c "1" ≡ c "1"
          , c "1" ≡ c "1" → c "1" ≡ c "1" → c "1" ≡ c "1" ∧ c "1" ≡ c "1"
          , c "1" ≡ c "1" 
          ])

      it ("parse just-or.proof") $ do
        let phi = Rel "=" [Const "1", Const "1"]
        parse justOr `shouldBe` (sig_empty { constants = ["1"]}, [],
          [ c "1" ≡ c "1" ∨ c "1" ≡ c "1" 
          , c "1" ≡ c "1" → c "1" ≡ c "1" ∨ c "1" ≡ c "1" 
          , c "1" ≡ c "1" 
          ])

      it ("Can parse equality of any two terms") $ do
        property $ \t -> let cs = getConstants t in
            parse ("constants: " ++ Data.List.intercalate "," cs ++ " |- " ++ show t ++ " = " ++ show t) ==
              (sig_empty {constants = cs}, [], [t ≡ t])


