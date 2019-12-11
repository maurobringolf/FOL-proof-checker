module TestUtils where

import Test.QuickCheck

import Signature
import Formula
import Term

import qualified Data.Char(isAsciiLower, isUpper)

instance Arbitrary Formula where
  arbitrary = oneof [ generateEq ]

instance Arbitrary Term where
  arbitrary = genTerm 1

-- Generates a term of maximum height `n`
genTerm :: Int -> Gen Term
genTerm n = oneof ([ generateConst, generateVar ] ++
             (if n > 0 then [generateFApp (n-1)] else []))

generateEq :: Gen Formula
generateEq = do n <- getSize
                t1 <- arbitrary
                t2 <- arbitrary
                return $ Eq t1 t2

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

