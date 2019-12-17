module TestUtils where

import Test.QuickCheck

import Signature
import Formula
import Term

import qualified Data.Char(isAsciiLower, isUpper)

infixr 5 ∧
(∧) = And

infixr 4 ∨
(∨) = Or

infixr 3 →
(→) = Imp

infixr 6 ≡
(≡) τ σ = Rel "=" [τ, σ]

c = Const
v = Var

instance Arbitrary Formula where
  arbitrary = do n <- getSize
                 oneof $ [ generateEq ] ++ (if n > 0 then [generateFA, generateEX , generateNot, generateAnd ,generateOr, generateImp ] else [])

instance Arbitrary Term where
  arbitrary = genTerm 1

smaller :: Gen a -> Gen a
smaller = scale (\n -> n `div` 2)

-- Generates a term of maximum height `n`
genTerm :: Int -> Gen Term
genTerm n = oneof ([ generateConst, generateVar ] ++
             (if n > 0 then [generateFApp (n-1)] else []))

generateEq :: Gen Formula
generateEq = do t1 <- arbitrary
                t2 <- arbitrary
                return $ Rel "=" [t1, t2]

generateFA :: Gen Formula
generateFA = do f <- smaller arbitrary
                x <- generateSymbol
                return $ FA x f

generateNot :: Gen Formula
generateNot = do f <- smaller arbitrary
                 return $ Not f

generateAnd :: Gen Formula
generateAnd = do f1 <- smaller arbitrary
                 f2 <- smaller arbitrary
                 return $ And f1 f2

generateOr :: Gen Formula
generateOr = do f1 <- smaller arbitrary
                f2 <- smaller arbitrary
                return $ And f1 f2

generateImp :: Gen Formula
generateImp = do f1 <- smaller arbitrary
                 f2 <- smaller arbitrary
                 return $ Imp f1 f2
                  
generateEX :: Gen Formula
generateEX = do f <- smaller arbitrary
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

