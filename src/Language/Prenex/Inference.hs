{-# LANGUAGE
    TypeFamilies
  , MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Language.Prenex.Inference where

import Language.Prenex.Inference.Types
import Language.Prenex.Inference.Class

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Maybe


mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return (s1 `composeSubst` s2)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt TInt = return nullSubst
mgu TBool TBool = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++
                         " vs. " ++ show t2


newTyVar :: MonadState Integer m => String -> m Type
newTyVar name = do
  suffix <- get
  put $ suffix+1
  return $ TVar $ name ++ show suffix

instantiate :: MonadState Integer m => Scheme -> m Type
instantiate (Scheme hs b) = do
  nhs <- mapM (\_ -> newTyVar "a") hs
  let s = M.fromList (zip hs nhs)
  return $ apply s b

generalize :: S.Set String -> Type -> Scheme
generalize m b = Scheme hs b
  where hs = S.toList (fv b `S.difference` fv env)

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2   = fmap (apply s1) s2 `M.union` s1

-- activeVars :: Constr -> S.Set String

bu :: MonadState Integer m => Set.Set String -> Exp -> m (Assum, ConstrSet, Type)
bu m (EVar n) = do
  b <- newTyVar "b"
  return ( [(n, b)]
         , []
         , b )
bu m (ELit (LInt _)) = do
  b <- newTyVar "b"
  return ( []
         , [CEqui b TInt]
         , b )
bu m (ELit (LBool _)) = do
  b <- newTyVar "b"
  return ( []
         , [CEqui b TBool]
         , b )
bu m (EApp e1 e2) = do
  (a1, c1, t1) <- bu m e1
  (a2, c2, t2) <- bu m e2
  b <- newTyVar "b"
  return ( a1 ++ a2
         , c1 ++ c2 ++ [CEqui t1 (TFun t2 b)]
         , b )
bu m (EAbs x body) = do
  b@(TVar vn) <- newTyVar "b"
  (a, c, t) <- bu (vn `Set.insert` m) body
  return ( a `removeAssum` x
         , c ++ [CEqui t' b | (x', t') <- a, x == x']
         , TFun b t )
bu m (ELet x e1 e2) = do
  (a1, c1, t1) <- bu m e1
  (a2, c2, t2) <- bu (x `Set.delete` m) e2
  return ( a1 ++ removeAssum a2 x
         , c1 ++ c2 ++ [CImpl t' m t1 | (x', t') <- a2, x' == x]
         , t2 )

removeAssum [] _ = []
removeAssum ((n', _) : as) n | n == n' = removeAssum as n
removeAssum (a:as) n = a : removeAssum as n
