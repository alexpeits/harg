{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides type-level functions that need proofs to work
-- properly.
module Options.Harg.Het.Proofs
  ( type (++),
    Proof (..),
    hgcastWith,
  )
where

import Data.Kind (Type)
import Data.Type.Equality

-- | Same as 'Data.Type.Equality.gcastWith' but for heterogeneous propositional
-- equality

#if __GLASGOW_HASKELL__ < 900
hgcastWith ::
  forall k k' (a :: k) (b :: k') (r :: Type).
  (a :~~: b) ->
  (a ~~ b => r) ->
  r
#else
hgcastWith ::
  forall {k} {k'} (a :: k) (b :: k') (r :: Type).
  (a :~~: b) ->
  (a ~~ b => r) ->
  r
#endif
hgcastWith HRefl x = x

-- * Concatenation of type-level lists

-- | Append two type-level lists
--
-- @
-- > :kind! '[Int, Bool] ++ '[Char, Maybe Int]
-- '[Int, Bool, Char, Maybe Int]
-- @
type family (xs :: [k]) ++ (ts :: [k]) = (res :: [k]) where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Proof that appending an empty list to any list has no effect on the latter.
class ProofNil xs where
  proofNil :: xs ++ '[] :~~: xs

instance ProofNil '[] where
  proofNil = HRefl

instance ProofNil xs => ProofNil (x ': xs) where
  proofNil = hgcastWith (proofNil @xs) HRefl

-- | Proof that appending two lists is the same as appending the first element
-- of the second list to the first one, and then appending the rest.
class Proof xs y zs where
  proof :: xs ++ (y ': zs) :~~: (xs ++ '[y]) ++ zs

instance ProofNil (xs ++ '[y]) => Proof (x ': xs) y '[] where
  proof = hgcastWith (proofNil @(xs ++ '[y])) HRefl

instance Proof '[] y zs where
  proof = HRefl

-- | Induction on the tail of the list
instance Proof xs y (z ': zs) => Proof (x ': xs) y (z ': zs) where
  proof :: x ': (xs ++ (y ': z ': zs)) :~~: x ': ((xs ++ '[y]) ++ (z ': zs))
  proof = hgcastWith (proof @xs @y @(z ': zs)) HRefl

instance Proof '[] y '[] where
  proof = HRefl
