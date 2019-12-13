module TestUtils where

import Test.QuickCheck

import Signature
import Formula
import Term

import qualified Data.Char(isAsciiLower, isUpper)

instance Arbitrary Formula where
  arbitrary = oneof [ generateEq, generateFA, generateEX, generateNot, generateAnd, generateOr, generateImp ]

instance Arbitrary Term where
  arbitrary = genTerm 1

-- Generates a term of maximum height `n`
genTerm :: Int -> Gen Term
genTerm n = oneof ([ generateConst, generateVar ] ++
             (if n > 0 then [generateFApp (n-1)] else []))

generateEq :: Gen Formula
generateEq = do t1 <- arbitrary
                t2 <- arbitrary
                return $ Eq t1 t2

generateFA :: Gen Formula
generateFA = do f <- arbitrary
                x <- generateSymbol
                return $ FA x f

generateNot :: Gen Formula
generateNot = do f <- arbitrary
                 return $ Not f

generateAnd :: Gen Formula
generateAnd = do f1 <- arbitrary
                 f2 <- arbitrary
                 return $ And f1 f2

generateOr :: Gen Formula
generateOr = do f1 <- arbitrary
                f2 <- arbitrary
                return $ And f1 f2

generateImp :: Gen Formula
generateImp = do f1 <- arbitrary
                 f2 <- arbitrary
                 return $ Imp f1 f2
                  
generateEX :: Gen Formula
generateEX = do f <- arbitrary
                x <- generateSymbol
                return $ EX x f

generateConst :: Gen Term
generateConst = do c <- suchThat arbitrary (\c -> Data.Char.isUpper c)
                   s <- generateSymbol
                   return $ Const (c:s)

generateVar :: Gen Term
generateVar = do s <- generateSymbol
                 return $ Var s

generateSymbol :: Gen Symbol
generateSymbol = suchThat arbitrary (\s -> length s <= 5 && not (null s) && all Data.Char.isAsciiLower s)

generateFApp :: Int -> Gen Term
generateFApp n = do f <- generateSymbol
                    m <- choose (1,5)
                    ts <- vectorOf m (genTerm (n - 1))
                    return $ FApp f ts

