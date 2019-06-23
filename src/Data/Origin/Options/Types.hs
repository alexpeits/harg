{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Origin.Options.Types where

import           Data.Coerce          (Coercible, coerce)
import           Data.Kind            (Type)
import           GHC.Generics         (Generic)

import qualified Data.Barbie          as B
import qualified Data.Functor.Product as P
import qualified Data.Generic.HKD     as HKD
import qualified Data.Validation      as V


data Opt a
  = Opt
      { _optLong    :: String
      , _optShort   :: Maybe Char
      , _optHelp    :: String
      , _optMetavar :: Maybe String
      , _optEnvVar  :: Maybe String
      , _optDefault :: Maybe a
      , _optParser  :: String -> Either String a
      }
  deriving Functor

-- | Existentially qualified 'Opt', in order to be able to extract information
-- not related to the wrapped type
data SomeOpt where
  SomeOpt :: Opt a -> SomeOpt

newtype OptValue a
  = OptValue
      { _getOptValueValidation :: V.Validation OptError a
      }
  deriving (Functor, Foldable, Traversable)
  deriving newtype Applicative

instance Semigroup (OptValue a) where
  l@(OptValue vl) <> r
    = case vl of
        V.Failure (OptNotPresent _) -> r
        _                           -> l

data OptError
  = OptNotPresent [SomeOpt]
  | OptInvalid [OptInvalidDetail]

instance Semigroup OptError where
  OptNotPresent l  <> OptNotPresent r  = OptNotPresent (l <> r)
  OptInvalid l     <> OptInvalid r     = OptInvalid (l <> r)
  l@(OptInvalid _) <> _                = l
  _                <> r@(OptInvalid _) = r

data OptInvalidDetail
  = OptInvalidDetail
      { _oidOpt     :: SomeOpt
      , _oidDetail  :: String
      }

toOptNotPresent :: Opt a -> OptValue a
toOptNotPresent
  = OptValue
  . V.Failure
  . OptNotPresent
  . pure
  . SomeOpt

toOptInvalid :: Opt a -> String -> OptValue a
toOptInvalid opt
  = OptValue
  . V.Failure
  . OptInvalid
  . pure
  . OptInvalidDetail (SomeOpt opt)

newtype Barbie (barbie :: (Type -> Type) -> Type) (f :: Type -> Type)
  = Barbie (barbie f)
  deriving newtype (Generic, B.ProductB, B.FunctorB)

instance (B.FunctorB b, B.ProductB b) => Semigroup (Barbie b OptValue) where
  l <> r = B.bmap (\(P.Pair x y) -> x <> y) (l `B.bprod` r)

-- Arg
newtype Arg (b :: Type) (f :: Type -> Type)
  = Arg
      { getArg :: f b
      }

arg :: f b -> Arg b f
arg = Arg

deriving instance (Show b, Show (f b)) => Show (Arg b f)
deriving newtype instance Generic (f b) => Generic (Arg b f)
deriving via (Barbie (Arg b) OptValue)
  instance ( B.FunctorB (Arg b)
           , B.ProductB (Arg b)
           ) => Semigroup (Arg b OptValue)

instance B.FunctorB (Arg b) where
  bmap nat (Arg p) = Arg (nat p)

instance B.ProductB (Arg b) where
  bprod (Arg l) (Arg r) = Arg (P.Pair l r)
  buniq = Arg

instance B.TraversableB (Arg b) where
  btraverse nat (Arg p) = Arg <$> nat p

-- Nested
newtype Nested (b :: Type) (f :: Type -> Type)
  = Nested (HKD.HKD b f)

type family Nest (a :: Type) (f :: Type -> Type) = (res :: Type) | res -> a where
  Nest (a -> b)      f = a -> Nest b f
  Nest (HKD.HKD a f) f = Nested a f

nested
  :: forall b f k l.
     ( HKD.Build b f k
     , Coercible (HKD.HKD b f) (Nested b f)
     , Coercible k (Nest k f)
     )
  => Nest k f
nested = coerce @k @(Nest k f) hkd
  where hkd = HKD.build @b @f @k

getNested
  :: ( Applicative f
     , Generic b
     , HKD.Construct f b
     )
  => Nested b f
  -> f b
getNested (Nested hkd) = HKD.construct hkd

deriving newtype instance Generic (HKD.HKD b f) => Generic (Nested b f)
deriving newtype instance B.FunctorB (HKD.HKD b) => B.FunctorB (Nested b)
deriving newtype instance B.ProductB (HKD.HKD b) => B.ProductB (Nested b)
deriving via (Barbie (Nested b) OptValue)
  instance ( B.FunctorB (Nested b)
           , B.ProductB (Nested b)
           ) => Semigroup (Nested b OptValue)

instance (B.TraversableB (HKD.HKD b)) => B.TraversableB (Nested b) where
  btraverse nat (Nested hkd) = Nested <$> B.btraverse nat hkd
