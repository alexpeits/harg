{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Options.Harg.Nested where

import           Data.Coerce           (Coercible, coerce)
import           Data.Kind             (Type)
import           GHC.Generics          (Generic)

import qualified Data.Aeson            as JSON
import qualified Data.Barbie           as B
import qualified Data.Generic.HKD      as HKD

import           Options.Harg.Het.Prod


-- Orphan HKD FromJSON instance
instance JSON.GFromJSON JSON.Zero (HKD.HKD_ f structure)
    => JSON.FromJSON (HKD.HKD structure f) where
  parseJSON
    = fmap HKD.HKD
    . JSON.gParseJSON JSON.defaultOptions JSON.NoFromArgs

newtype Nested (b :: Type) (f :: Type -> Type)
  = Nested (HKD.HKD b f)

type family Nest
    (x :: Type -> (Type -> Type) -> Type)
    (a :: Type)
    (f :: Type -> Type)
    = (res :: Type) | res -> a where
  Nest x (a -> b)      f = a -> Nest x b f
  Nest x (HKD.HKD a f) f = x a f

nested
  :: forall b f k.
     ( HKD.Build b f k
     , Coercible (HKD.HKD b f) (Nested b f)
     , Coercible k (Nest Nested k f)
     )
  => Nest Nested k f
nested = coerce @k @(Nest Nested k f) hkd
  where hkd = HKD.build @b @f @k

getNested
  :: HKD.Construct f b
  => Nested b f
  -> f b
getNested (Nested hkd) = HKD.construct hkd

deriving newtype instance Generic (HKD.HKD b f) => Generic (Nested b f)
deriving newtype instance JSON.FromJSON (HKD.HKD b f) => JSON.FromJSON (Nested b f)

deriving newtype instance B.FunctorB (HKD.HKD b) => B.FunctorB (Nested b)
deriving newtype instance B.ProductB (HKD.HKD b) => B.ProductB (Nested b)

instance (B.TraversableB (HKD.HKD b)) => B.TraversableB (Nested b) where
  btraverse nat (Nested hkd) = Nested <$> B.btraverse nat hkd

newtype TNested
    (t :: k)
    (b :: Type)
    (f :: Type -> Type)
  = TNested (Tagged t (HKD.HKD b) f)

deriving newtype instance Generic (TNested t b f)
deriving newtype instance JSON.FromJSON (HKD.HKD b f) => JSON.FromJSON (TNested t b f)

deriving newtype instance B.FunctorB (HKD.HKD b) => B.FunctorB (TNested t b)
deriving newtype instance B.ProductB (HKD.HKD b) => B.ProductB (TNested t b)

instance (B.TraversableB (HKD.HKD b)) => B.TraversableB (TNested t b) where
  btraverse nat (TNested x) = TNested <$> B.btraverse nat x

tnested
  :: forall b f k t.
     ( HKD.Build b f k
     , Coercible (HKD.HKD b f) (TNested t b f)
     , Coercible k (Nest (TNested t) k f)
     )
  => Nest (TNested t) k f
tnested = coerce @k @(Nest (TNested t) k f) hkd
  where hkd = HKD.build @b @f @k

getTNested
  :: HKD.Construct f b
  => TNested t b f
  -> f b
getTNested (TNested (Tagged hkd)) = HKD.construct hkd
