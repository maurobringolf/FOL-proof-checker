module ParserSpec where

import Test.Hspec
import Test.QuickCheck

import qualified Signature as Sig
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
        parse simpleMP `shouldBe` (Sig.empty { Sig.constants = ["A", "B", "C", "D"]}
                                  , map Literal [c "A" ≡ c "B" → c "C" ≡ c "D"
                                    , c "A" ≡ c "B"
                                    ]
                                  , [ c "C" ≡ c "D"
                                    , c "A" ≡ c "B"
                                    , c "A" ≡ c "B" → c "C" ≡ c "D"
                                    ])

      it ("parse just-equality.proof") $ do
        parse justEq `shouldBe` (Sig.empty, [], [v "x" ≡ v "y"])
        
      it ("parse fapp-equality.proof") $ do
        parse fAppEq `shouldBe` (Sig.empty { Sig.functions = [("f", 1), ("g", 1)], Sig.constants = ["0", "1"] }, [], [FApp "f" [Const "0"] ≡ FApp "g" [Const "1"]])

      it ("parse L9.proof") $ do
        parse l9 `shouldBe` (Sig.empty { Sig.relations = [ ("φ", 0), ("ψ", 0)] }, [], reverse [
          Not (v "x" ≡ v "x") → v "x" ≡ v "x" → v "x" ≡ v "y" 
          , (Not (Rel "φ" [])) → (Rel "φ" []) → (Rel "ψ" [])
          ])

      it ("parse just-and.proof") $ do
        parse justAnd `shouldBe` (Sig.empty { Sig.constants = [ "1" ]}, [],
          [ c "1" ≡ c "1" ∧ c "1" ≡ c "1" 
          , c "1" ≡ c "1" → c "1" ≡ c "1" ∧ c "1" ≡ c "1"
          , c "1" ≡ c "1" → c "1" ≡ c "1" → c "1" ≡ c "1" ∧ c "1" ≡ c "1"
          , c "1" ≡ c "1" 
          ])

      it ("parse just-or.proof") $ do
        let phi = Rel "=" [Const "1", Const "1"]
        parse justOr `shouldBe` (Sig.empty { Sig.constants = ["1"]}, [],
          [ c "1" ≡ c "1" ∨ c "1" ≡ c "1" 
          , c "1" ≡ c "1" → c "1" ≡ c "1" ∨ c "1" ≡ c "1" 
          , c "1" ≡ c "1" 
          ])

      it ("Can parse equality of any two terms") $ do
        property $ \t -> let cs = getConstants t
                             fs = getFunctions t in
            parse ("#constants: " ++ Data.List.intercalate "," cs ++ " #functions: " ++ Data.List.intercalate "," (map (\(f,n) -> f ++ "(" ++ show n ++ ")" ) fs) ++ " |- " ++ show t ++ " = " ++ show t) ==
              (Sig.empty {Sig.constants = cs, Sig.functions = fs}, [], [t ≡ t])


