{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Options.Harg.Het.HList
  ( MapAssocList (..),
    AssocListF (..),
    (:+),
    pattern (:+),
    (:->),
  )
where

import qualified Barbies as B
import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

-- | A heterogeneous list that holds higher-kinded types and the associated
-- type constructor, along with a type level list of 'Symbol's that act
-- as tags for each type.
data
  AssocListF
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (f :: Type -> Type)
  where
  ANil :: AssocListF '[] '[] f
  ACons :: x f -> AssocListF ts xs f -> AssocListF (t ': ts) (x ': xs) f

-- | Helper type-level function to construct an 'AssocList' which is not
-- yet applied to the type constructor that needs to be fully applied.
--
-- @
--   type Config
--     =  "run" :-> RunConfig
--     :+ "test" :-> TestConfig
-- @
--
-- @Config@ above has type @(Type -> Type) -> Type@, and requires a type
-- like 'Opt' to be fully applied.
type family l :+ r = (res :: (Type -> Type) -> Type) where
  (tl :-> vl) :+ (tr :-> vr) = AssocListF '[tl, tr] '[vl, vr]
  (tl :-> vl) :+ AssocListF ts vs = AssocListF (tl ': ts) (vl ': vs)
  l :+ r =
    TypeError
      ( 'Text "Invalid type for tagged options. Construct like this:"
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
  -- | Apply a function to all higher-kinded types in an 'AssocList'.
  mapAssocList ::
    (forall a. B.FunctorB a => a f -> a g) ->
    AssocListF ts as f ->
    AssocListF ts as g

instance MapAssocList '[] where
  mapAssocList _ ANil =
    ANil

instance (MapAssocList as, B.FunctorB a) => MapAssocList (a ': as) where
  mapAssocList f (ACons x xs) =
    ACons (f x) (mapAssocList f xs)
