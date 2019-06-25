{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Het where

import           Data.Kind                   (Type)
import           Data.Type.Equality

import           Options.Harg.Het.AssocList
import           Options.Harg.Het.HList
import           Options.Harg.Het.Variant

class MapVariantF (xs :: [(Type -> Type) -> Type]) where
  mapVariantF :: VariantF xs g -> HListF xs f -> VariantF xs f

instance MapVariantF xs => MapVariantF (x ': xs) where
  mapVariantF (HereF _) (HConsF x _) = HereF x
  mapVariantF (ThereF v) (HConsF _ l) = ThereF $ mapVariantF v l

instance MapVariantF '[] where
  mapVariantF _ _ = error "Impossible: empty variant"

assocToHListF :: AssocListF ts xs f -> HListF xs f
assocToHListF ANil = HNilF
assocToHListF (ACons x xs) = HConsF x $ assocToHListF xs

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
