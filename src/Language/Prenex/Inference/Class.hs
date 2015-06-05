{-# LANGUAGE
    KindSignatures
  , TypeFamilies
  , MultiParamTypeClasses
  #-}

module Language.Prenex.Inference.Class where


class Bindable a name where
  type Set a :: * -> *
  fv :: a -> Set a name -- ^ Returns free variables from @a@

class Replaceable a name where
  type Subst a :: * -> * -> *
  type Base a :: *
  apply :: Subst a name (Base a) -> a -> a -- ^ Applies a substitution to @a@
