{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Het.Proofs where

import           Data.Kind                   (Type)
import           Data.Type.Equality

-- proofs
type family (xs :: [k]) ++ (ts :: [k]) = (res :: [k]) where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- same as `gcastWith` but for heterogeneous propositional equality
hgcastWith
  :: forall (a :: k) (b :: k') (r :: Type).
     (a :~~: b) -> (a ~~ b => r) -> r
hgcastWith HRefl x = x

class ProofNil xs where
  proofNil :: xs ++ '[] :~~: xs

instance ProofNil '[] where
  proofNil = HRefl

instance ProofNil xs => ProofNil (x ': xs) where
  proofNil = hgcastWith (proofNil @xs) HRefl

class Proof
    (xs :: [(Type -> Type) -> Type])
    (y :: (Type -> Type) -> Type)
    (zs :: [(Type -> Type) -> Type]) where
  proof :: xs ++ (y ': zs) :~~: (xs ++ '[y]) ++ zs

instance ProofNil (xs ++ '[y]) => Proof (x ': xs) y '[] where
  proof = hgcastWith (proofNil @(xs ++ '[y])) HRefl

instance Proof '[] y zs where
  proof = HRefl

instance Proof xs y (z ': zs) => Proof (x ': xs) y (z ': zs) where
  -- Induction on the cdr of the list (everything after the `x`)
  proof
    ::   (x ': (xs ++ (y ': z ': zs)))
    :~~: (x ': ((xs ++ '[y]) ++ (z ': zs)))
  proof = hgcastWith (proof @xs @y @(z ': zs)) HRefl
