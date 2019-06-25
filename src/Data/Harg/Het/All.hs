{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Harg.Het.All where

import           Data.Kind                   (Type, Constraint)

type family
  All
    (c :: k -> Constraint)
    (xs :: [k])
    :: Constraint where
  All _ '[]       = ()
  All c (x ': xs) = (c x, All c xs)

type family
  AllF
    (c :: k -> Constraint)
    (xs :: [(Type -> Type) -> Type])
    (f :: Type -> Type)
    :: Constraint where
  AllF _ '[] _       = ()
  AllF c (x ': xs) f = (c (x f), AllF c xs f)
