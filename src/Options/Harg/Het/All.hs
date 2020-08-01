{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Options.Harg.Het.All
  ( All,
  )
where

import Data.Kind (Constraint, Type)

-- | @All c xs@ returns a constraint which is constructed by
-- applying @c@ to all the types in @xs@.
type family
  All
    (c :: k -> Constraint)
    (xs :: [k]) ::
    Constraint
  where
  All _ '[] = ()
  All c (x ': xs) = (c x, All c xs)

-- | @AllF c xs f@ is similar to 'All', but types in @xs@ have the kind @(Type
-- -> Type) -> Type@ so they require an extra @f :: Type -> Type@ in order to
-- be of fully saturated.
type family
  AllF
    (c :: k -> Constraint)
    (xs :: [(Type -> Type) -> Type])
    (f :: Type -> Type) ::
    Constraint
  where
  AllF _ '[] _ = ()
  AllF c (x ': xs) f = (c (x f), AllF c xs f)
