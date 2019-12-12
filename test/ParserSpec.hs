module ParserSpec where

import Test.Hspec
import Test.QuickCheck

import Signature
import Term
import Formula
import Proof
import Parser

import TestUtils

import Data.Char(isAsciiLower)

parseTests :: IO ()
parseTests = do
  simpleMP <- readFile "test/proofs/correct/simple-modus-ponens.proof"
  justEq   <- readFile "test/proofs/incorrect/just-equality.proof"
  fAppEq   <- readFile "test/proofs/incorrect/fapp-equality.proof"
  justAnd  <- readFile "test/proofs/correct/just-and.proof"
  justOr  <- readFile "test/proofs/correct/just-or.proof"

  hspec $ do
    describe "Parser tests" $ do
      it ("parse simple-modus-ponens.proof") $ do
        parse simpleMP `shouldBe` ( sig_empty
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
        parse fAppEq `shouldBe` (sig_empty, [], [Eq (FApp "f" [Const "0"]) (FApp "g" [Const "1"])])

      it ("parse just-and.proof") $ do
        let phi = Eq (Const "1") (Const "1")
        parse justAnd `shouldBe` (sig_empty, [],
          [ And phi phi
          , Imp phi (And phi phi)
          , Imp phi (Imp phi (And phi phi))
          , phi
          ])

      it ("parse just-or.proof") $ do
        let phi = Eq (Const "1") (Const "1")
        parse justOr `shouldBe` (sig_empty, [],
          [ Or phi phi
          , Imp phi (Or phi phi)
          , phi
          ])

      it ("Can parse equality of any two terms") $ do
        property $ \t -> parse ("|- " ++ show t ++ " = " ++ show t) ==
          (sig_empty, [], [Eq t t])


