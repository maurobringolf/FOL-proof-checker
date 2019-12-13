module ParserSpec where

import Test.Hspec
import Test.QuickCheck

import Signature
import Term
import Formula
import Proof
import Parser
import AstUtils

import TestUtils

import Data.Char(isAsciiLower)
import Data.List(intercalate)

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
                                  , [ Imp (Eq (Const "A") (Const "B")) (Eq (Const "C") (Const "D")) -- A = B -> C = D
                                    , Eq (Const "A") (Const "B")                                    -- A = B
                                    ]
                                  , [ Eq (Const "C") (Const "D")                                    -- C = D
                                    , Eq (Const "A") (Const "B")                                    -- A = B
                                    , Imp (Eq (Const "A") (Const "B")) (Eq (Const "C") (Const "D")) -- A = B -> C = D
                                    ])

      it ("parse just-equality.proof") $ do
        parse justEq `shouldBe` (sig_empty, [], [Eq (Var "x") (Var "y")])
        
      it ("parse fapp-equality.proof") $ do
        parse fAppEq `shouldBe` (sig_empty { constants = ["0", "1"] }, [], [Eq (FApp "f" [Const "0"]) (FApp "g" [Const "1"])])

      it ("parse L9.proof") $ do
        parse l9 `shouldBe` (sig_empty, [], [
        -- ¬(x = x) -> x = x -> x = y
          Imp (Not (Eq (Var "x") (Var "x"))) (Imp (Eq (Var "x") (Var "x")) (Eq (Var "x") (Var "y")))
          ])

      it ("parse just-and.proof") $ do
        let phi = Eq (Const "1") (Const "1")
        parse justAnd `shouldBe` (sig_empty { constants = [ "1" ]}, [],
          [ And phi phi
          , Imp phi (And phi phi)
          , Imp phi (Imp phi (And phi phi))
          , phi
          ])

      it ("parse just-or.proof") $ do
        let phi = Eq (Const "1") (Const "1")
        parse justOr `shouldBe` (sig_empty { constants = ["1"]}, [],
          [ Or phi phi
          , Imp phi (Or phi phi)
          , phi
          ])

      it ("Can parse equality of any two terms") $ do
        property $ \t -> let cs = getConstants t in
            parse ("constants: " ++ Data.List.intercalate "," cs ++ " |- " ++ show t ++ " = " ++ show t) ==
              (sig_empty {constants = cs}, [], [Eq t t])


