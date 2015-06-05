{-# LANGUAGE
    TypeFamilies
  , MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Language.Prenex.Inference.Types where

import Language.Prenex.Inference.Class

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Maybe


data Expr =
    EVar { eVarName :: String }
  | EAbs { eAbsHead :: String
         , eAbsBody :: Expr }
  | EApp { eAppFunc :: Expr
         , eAppArgu :: Expr }
  deriving (Show, Eq)

data Type =
    TVar { tVarName :: String }
  | TFun { tVarHead :: Type
         , tVarBody :: Type }
  deriving (Show, Eq)

instance Bindable Type String where
  type Set Type = S.Set
  fv (TVar n) = S.singleton n
  fv (TFun h b) = fv h `S.union` fv b

instance Replaceable Type String where
  type Subst Type = M.Map
  type Base Type = Type
  apply s t@(TVar n) = fromMaybe t $ M.lookup n s
  apply s (TFun h b) = TFun (apply s h) (apply s b)

data Scheme =
  Scheme { sHead :: [String]
         , sBody :: Type }

instance Bindable Scheme String where
  type Set Scheme = S.Set
  fv (Scheme hs b) = S.difference (fv b) $ S.fromList hs

instance Replaceable Scheme String where
  type Subst Scheme = M.Map
  type Base Scheme = Type
  apply s (Scheme hs b) = Scheme hs $ apply s' b
    where
    s' = foldr M.delete s hs

data Constr =
    CEqui { cEquiLeft :: Type
          , cEquiRight :: Type }
  | CImpl { cImplLeft :: Type
          , cImplMono :: S.Set String
          , cImplRight :: Type }
  | CExpl { cExplLeft :: Type
          , cExplRight :: Scheme }

type Assum = [(String, Type)]
type ConstrSet = [Constr]

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
