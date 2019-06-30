{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Het.HList where

import           Data.Kind             (Type)
import           Data.Functor.Identity (Identity)

import qualified Data.Barbie           as B

import           Options.Harg.Het.All


data HListF (xs :: [(Type -> Type) -> Type]) (f :: Type -> Type) where
  HNilF  :: HListF '[] f
  HConsF :: x f -> HListF xs f -> HListF (x ': xs) f

type family
    (l :: (Type -> Type) -> Type) :* (r :: (Type -> Type) -> Type)
    = (res :: (Type -> Type) -> Type) where
  l :* HListF rs = HListF (l ': rs)
  l :* r = HListF '[l, r]

pattern (:*) :: x f -> HListF xs f -> HListF (x ': xs) f
pattern x :* xs = HConsF x xs

infixr 4 :*

deriving instance AllF Show xs Identity => Show (HListF xs Identity)

instance ( B.FunctorB x
         , B.FunctorB (HListF xs)
         ) => B.FunctorB (HListF (x ': xs)) where
  bmap nat (HConsF x xs)
    = HConsF (B.bmap nat x) (B.bmap nat xs)

instance B.FunctorB (HListF '[]) where
  bmap _ HNilF = HNilF

instance ( B.FunctorB x
         , B.FunctorB (HListF xs)
         , B.ProductB x
         , B.ProductB (HListF xs)
         ) => B.ProductB (HListF (x ': xs)) where
  (HConsF x xs) `bprod` (HConsF y ys) = HConsF (x `B.bprod` y) (xs `B.bprod` ys)
  buniq fa = HConsF (B.buniq fa) (B.buniq fa)

instance B.ProductB (HListF '[]) where
  HNilF `bprod` HNilF = HNilF
  buniq _ = HNilF

instance ( B.TraversableB x
         , B.TraversableB (HListF xs)
         ) => B.TraversableB (HListF (x ': xs)) where
  btraverse nat (HConsF x xs)
    = HConsF <$> B.btraverse nat x <*> B.btraverse nat xs

instance B.TraversableB (HListF '[]) where
  btraverse _ HNilF = pure HNilF
