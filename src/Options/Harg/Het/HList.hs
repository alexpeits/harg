{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Het.HList where

import           Data.Kind               (Type)
import           GHC.TypeLits            (ErrorMessage(..), TypeError, Symbol)

import qualified Data.Barbie             as B

import           Options.Harg.Het.All
import           Options.Harg.Het.Proofs


data AssocListF
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (f :: Type -> Type) where
  ANil  :: AssocListF '[] '[] f
  ACons :: x f -> AssocListF ts xs f -> AssocListF (t ': ts) (x ': xs) f

type family l :+ r = (res :: (Type -> Type) -> Type) where
  (tl :-> vl) :+ (tr :-> vr) = AssocListF '[tl, tr] '[vl, vr]
  (tl :-> vl) :+ AssocListF ts vs = AssocListF (tl ': ts) (vl ': vs)
  l :+ r
    = TypeError
    (    'Text "Invalid type for tagged options. Construct like this:"
    :$$: 'Text "type MyConfig"
    :$$: 'Text "  =  \"one\" :-> ConfigForOne"
    :$$: 'Text "  :+ \"two\" :-> ConfigForTwo"
    )

pattern (:+) :: x f -> AssocListF ts xs f -> AssocListF (t ': ts) (x ': xs) f
pattern x :+ xs = ACons x xs

infixr 4 :+

data (t :: Symbol) :-> (v :: (Type -> Type) -> Type) :: (Type -> Type) -> Type

infixr 5 :->

class MapAssocList (as :: [(Type -> Type) -> Type]) where
  mapAssocList :: (forall a. B.FunctorB a => a f -> a g) -> AssocListF ts as f -> AssocListF ts as g

instance MapAssocList '[] where
  mapAssocList _ ANil = ANil

instance (MapAssocList as, B.FunctorB a) => MapAssocList (a ': as) where
  mapAssocList f (ACons x xs) = ACons (f x) (mapAssocList f xs)


data HList (as :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

deriving instance (All Show xs) => Show (HList xs)

(+++) :: HList as -> HList bs -> HList (as ++ bs)
HNil +++ ys = ys
(HCons x xs) +++ ys = HCons x (xs +++ ys)
