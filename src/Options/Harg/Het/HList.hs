{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Het.HList where

import           Data.Kind    (Type)
import           GHC.TypeLits (ErrorMessage(..), TypeError, Symbol)

import qualified Data.Barbie  as B


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
  mapAssocList
    :: (forall a. B.FunctorB a => a f -> a g)
    -> AssocListF ts as f
    -> AssocListF ts as g

instance MapAssocList '[] where
  mapAssocList _ ANil
    = ANil

instance (MapAssocList as, B.FunctorB a) => MapAssocList (a ': as) where
  mapAssocList f (ACons x xs)
    = ACons (f x) (mapAssocList f xs)
