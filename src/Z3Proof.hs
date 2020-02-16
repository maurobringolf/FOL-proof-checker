module Z3Proof where

import Signature
import Term
import Formula
import AstUtils
import Context

import qualified Theory.Logical(axioms)
import qualified Data.Set
import Data.Map(empty, insert, lookup, Map)
import Data.Maybe(fromJust)

import Z3.Monad hiding (Context, Result)
import qualified Z3.Base(Result)

type Proof = [Formula]

data Result = Correct
            | Incorrect Context Formula

instance Show Result where
  show Correct = "Correct"
  show (Incorrect phis phi) = "Incorrect. Cannot infer: \n" ++ show phi

instance Eq Result where
  (==) Correct Correct = True
  (==) (Incorrect _ _) (Incorrect _ _) = True
  (==) _ _ = False

-- NOTE: `proof` is in reverse order, i.e. `head proof` is the proven formula
checkProofWithZ3 :: Signature -> Context -> Proof -> IO Result
checkProofWithZ3 sig nonLogicalAxioms proof = do (res, transcript) <- evalZ3 (script sig nonLogicalAxioms proof)
                                                 putStrLn transcript
                                                 return res

data Z3Context = Z3Context { z3constants :: Map Signature.Symbol AST 
                           , z3functions :: Map Signature.Symbol FuncDecl
                           , z3relations :: Map Signature.Symbol FuncDecl }

translateSignature :: Signature -> Z3 (Sort, Z3Context)
translateSignature Signature{ constants=cs, functions=fs, relations=rs, binary_functions=bs} = do

  -- Create sort for the domain
  domainName <- mkStringSymbol "domain"
  domainSort <- mkUninterpretedSort domainName

  -- Create constants
  z3cs <- translateConstants domainSort cs
  z3rs <- translateRelations domainSort rs
  z3fs <- translateFunctions domainSort (fs ++ map (\b -> (b,2)) bs)
  return  (domainSort, Z3Context { z3constants = z3cs
                                 , z3functions = z3fs
                                 , z3relations = z3rs })

translateConstants :: Sort -> [Signature.Symbol] -> Z3 (Map Signature.Symbol AST)
translateConstants s cs = case cs of
  [] -> return Data.Map.empty
  (c:cs) -> do c' <- mkStringSymbol c
               c'' <- mkConst c' s
               m <- translateConstants s cs
               return (insert c c'' m)

translateFunctions :: Sort -> [(Signature.Symbol, Int)] -> Z3 (Map Signature.Symbol FuncDecl)
translateFunctions s cs = case cs of
  [] -> return Data.Map.empty
  ((f, n) : fs) -> do f' <- mkStringSymbol f
                      f'' <- mkFuncDecl f' (replicate n s) s
                      m <- translateFunctions s fs
                      return (insert f f'' m)

translateRelations :: Sort -> [(Signature.Symbol, Int)] -> Z3 (Map Signature.Symbol FuncDecl)
translateRelations s cs = case cs of
  [] -> return Data.Map.empty
  ((f, n) : fs) -> do f' <- mkStringSymbol f
                      b <- mkBoolSort
                      f'' <- mkFuncDecl f' (replicate n s) b
                      m <- translateFunctions s fs
                      return (insert f f'' m)

runProof :: (Sort, Z3Context) -> Proof -> Z3 (Result, String)
runProof s proof = case proof of
  [] -> return (Correct, "")
  (f:fs) -> do (rBefore, tBefore) <- runProof s fs
               if rBefore == Correct then do
                 f' <- translateFormula s f
                 solverPush
                 notf' <- mkNot f'
                 assert notf'
                 r <- check
                 t <- solverToString
                 solverPop 1
                 return r
                 if r == Unsat then do
                   assert f' 
                   t' <- solverToString
                   return (Correct, t')
                 else
                   return $ (Incorrect [] f, t)
               else
                 return (rBefore, tBefore)

translateFormula :: (Sort, Z3Context) -> Formula -> Z3 AST
translateFormula ss@(s, ctxt) f = case f of

  FA x g -> do x' <- mkStringSymbol x
               g' <- translateFormula ss g
               f' <- mkForall [] [x'] [s] g'
               return f'

  EX x g -> do x' <- mkStringSymbol x
               g' <- translateFormula ss g
               f' <- mkExists [] [x'] [s] g'
               return f'

  Rel "=" [t1, t2] -> do t1' <- translateTerm ss t1
                         t2' <- translateTerm ss t2
                         mkEq t1' t2'

  Rel r args -> do args' <- mapM (translateTerm ss) args
                   case Data.Map.lookup r (z3relations ctxt) of
                     Nothing -> error r
                     Just r' -> mkApp r' args'

  Imp f1 f2 -> do f1' <- translateFormula ss f1
                  f2' <- translateFormula ss f2
                  mkImplies f1' f2'

  And f1 f2 -> do f1' <- translateFormula ss f1
                  f2' <- translateFormula ss f2
                  mkAnd [f1', f2']

  Or f1 f2 -> do f1' <- translateFormula ss f1
                 f2' <- translateFormula ss f2
                 mkOr [f1', f2']

  Not f -> do f' <- translateFormula ss f
              mkNot f'

translateTerm :: (Sort, Z3Context) -> Term -> Z3 AST
translateTerm ss@(s,ctxt) t = case t of
  
  Const c -> do c' <- mkStringSymbol c
                mkConst c' s

  Var x -> do x' <- mkStringSymbol x
              mkVar x' s

  FApp f args -> do args' <- mapM (translateTerm ss) args
                    case Data.Map.lookup f (z3functions ctxt) of
                      Nothing -> error f
                      Just f' -> mkApp f' args'

translateContext :: (Sort, Z3Context) -> Context -> Z3 ()
translateContext s ctxt = case ctxt of
  [] -> return ()
  (Literal f:fs) -> do f' <- translateFormula s f
                       assert f'
                       translateContext s fs
  (Schema f:fs) ->  do translateContext s fs
                       -- TODO: Not possible I think?

script :: Signature -> Context -> Proof -> Z3 (Result, String)
script sig ctxt proof = do

  (domainSort, z3ctxt) <- translateSignature sig

  translateContext (domainSort, z3ctxt) ctxt

  (r, transcript) <- runProof (domainSort, z3ctxt) proof
  return (r, transcript)


