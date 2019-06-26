{-# LANGUAGE AllowAmbiguousTypes  #-}
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
    (ys :: [(Type -> Type) -> Type]) where
  proof :: xs ++ (y ': ys) :~~: (xs ++ '[y]) ++ ys

instance ProofNil (xs ++ '[y]) => Proof (x ': xs) y '[] where
  proof = hgcastWith (proofNil @(xs ++ '[y])) HRefl

instance Proof '[] y ys where
  proof = HRefl
