import Test.Hspec
import Test.QuickCheck

import Signature
import Term
import Formula
import Proof

import Data.Char(isAlphaNum)

main :: IO ()
main = do
  quickCheck . verbose $ property $ \phi -> checkProof [phi] [phi] 

  hspec $ do
    describe "Term.wf_term" $ do
      it "`x` is well-formed" $ do
        wf_term sig_empty (Var "x") `shouldBe` True

      it "Any variable is well-formed" $
        property $ \s -> wf_term sig_empty (Var s)

      it "Any constant is well-formed if it is in the signature" $
        property $ \s -> wf_term (sig_empty {constants = [s]} ) (Const s)

      it "Any non-logical axiom is a proof of itself" $
        property $ \phi -> checkProof [phi] [phi] 

      it "φ -> ψ, φ |- ψ" $
        property $ \phi psi -> checkProof [Imp phi psi, phi] [Imp phi psi, phi, psi]


instance Arbitrary Formula where
  arbitrary = oneof [ generateEq ]

instance Arbitrary Term where
  arbitrary = genTerm 5


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
generateConst = do s <- generateSymbol
                   return $ Const s

generateVar :: Gen Term
generateVar = do s <- generateSymbol
                 return $ Var s

generateSymbol :: Gen Symbol
generateSymbol = suchThat arbitrary (\s -> length s <= 5 && not (null s) && all Data.Char.isAlphaNum s)

generateFApp :: Int -> Gen Term
generateFApp n = do f <- generateSymbol
                    m <- choose (1,5)
                    ts <- vectorOf m (genTerm (n - 1))
                    return $ FApp f ts


