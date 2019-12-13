{-# LANGUAGE MultiWayIf #-}

module AstUtils where

import Signature
import Term
import Formula

import Data.Maybe(catMaybes)
import qualified Data.Set as Set

getConstants :: Term -> [Symbol]
getConstants t = case t of
  Const c   -> [c]
  Var x     -> []
  FApp f ts -> concatMap getConstants ts

freeF :: Formula -> Set.Set Symbol
freeF f = case f of
  Eq t1 t2 -> freeT t1 `Set.union` freeT t2
  And f1 f2 -> freeF f1 `Set.union` freeF f2
  Or f1 f2 -> freeF f1 `Set.union` freeF f2
  Imp f1 f2 -> freeF f1 `Set.union` freeF f2
  Not f1 -> freeF f1
  Rel _ args -> Set.unions (map freeT args)
  EX x f -> Set.delete x $ freeF f
  FA x f -> Set.delete x $ freeF f

freeT :: Term -> Set.Set Symbol
freeT t = case t of
  Const c   -> Set.empty
  Var x     -> Set.singleton x
  FApp f ts -> Set.unions (map freeT ts)

substF :: Symbol -> Term -> Formula -> Formula
substF x t f = case f of
  Eq l r     -> Eq (substT x t l) (substT x t r)
  Rel r args -> Rel r $ map (substT x t) args
  FA y f'    -> if y == x then f else FA y (substF x t f')
  EX y f'    -> if y == x then f else EX y (substF x t f')
  Not f      -> Not (substF x t f)
  And l r    -> And (substF x t l) (substF x t r)
  Or l r     -> Or (substF x t l) (substF x t r)
  Imp l r    -> Imp (substF x t l) (substF x t r)

-- substT x t t' computes the substitution t'[x/t]
substT :: Symbol -> Term -> Term -> Term
substT x t t' = case t' of
  Const _     -> t'
  Var y       -> if x == y then t else t'
  FApp f args -> FApp f $ map (substT x t) args

-- finds a value t = findTau x f1 f2 such that f1[x/t] = f2
findTau :: Symbol -> Formula -> Formula -> Maybe Term
findTau x f1 f2 = case (f1, f2) of
  (FA y f1', FA z f2')       -> if | y == z && y /= x -> findTau x f1' f2'
                                   | otherwise        -> Nothing
  (EX y f1', EX z f2')       -> if | y == z && y /= x -> findTau x f1' f2'
                                   | otherwise        -> Nothing
  (Eq t11 t12, Eq t21 t22)   -> if | t11 == (Var x)   -> Just t21
                                   | t12 == (Var x)   -> Just t22
                                   | otherwise        -> let taus = Data.Maybe.catMaybes [findTauT x t11 t21, findTauT x t12 t22]
                                                       in
                                                       if | null taus -> Nothing
                                                          | otherwise -> Just (head taus)
  (And f11 f12, And f21 f22) -> let taus = Data.Maybe.catMaybes [findTau x f11 f21, findTau x f12 f22]
                                in
                                if | null taus -> Nothing
                                   | otherwise -> Just (head taus)
  (Or f11 f12, Or f21 f22)  -> let taus = Data.Maybe.catMaybes [findTau x f11 f21, findTau x f12 f22]
                                in
                                if | null taus -> Nothing
                                   | otherwise -> Just (head taus)
  (Imp f11 f12, Imp f21 f22) -> let taus = Data.Maybe.catMaybes [findTau x f11 f21, findTau x f12 f22]
                                in
                                if | null taus -> Nothing
                                   | otherwise -> Just (head taus)
  (Not f1', Not f2')         -> findTau x f1' f2'
  (_,_)                      -> Nothing

-- finds a value t'' = findTauT x t t' such that t[x/t''] = t'
findTauT :: Symbol -> Term -> Term -> Maybe Term
findTauT x t t' = case (t, t') of
  (Var y1, t')  -> if | y1 == x   -> Just t'
                      | otherwise -> Nothing
  (Const _, Const _) -> Nothing
  (FApp f1 args1, FApp f2 args2) -> if | f1 == f2 ->  let taus = Data.Maybe.catMaybes (zipWith (\a1 a2 -> findTauT x a1 a2) args1 args2)
                                                      in
                                                      if | null taus -> Nothing
                                                         | otherwise -> Just (head taus)
                                       | otherwise -> Nothing
  (_,_)              -> Nothing

