{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE UndecidableInstances       #-}
module Options.Harg.Types where

import           Data.Coerce          (Coercible, coerce)
import           Data.Kind            (Type)
import           GHC.Generics         (Generic)
import qualified Options.Applicative  as Optparse

import qualified Data.Barbie          as B
import qualified Data.Functor.Product as P
import qualified Data.Generic.HKD     as HKD


type OptParser a = String -> Either String a

data Opt a
  = Opt
      { _optLong    :: String
      , _optShort   :: Maybe Char
      , _optHelp    :: String
      , _optMetavar :: Maybe String
      , _optEnvVar  :: Maybe String
      , _optDefault :: Maybe a
      , _optParser  :: OptParser a
      , _optType    :: OptType a
      }
  deriving Functor

data OptType a
  = ArgOptType
  | FlagOptType a  -- active value
  deriving Functor

data ArgOpt a
  = ArgOpt
      { _aLong    :: String
      , _aShort   :: Maybe Char
      , _aHelp    :: String
      , _aMetavar :: Maybe String
      , _aEnvVar  :: Maybe String
      , _aDefault :: Maybe a
      , _aParser  :: OptParser a
      }

data FlagOpt a
  = FlagOpt
      { _sLong    :: String
      , _sShort   :: Maybe Char
      , _sHelp    :: String
      , _sEnvVar  :: Maybe String
      , _sDefault :: a
      , _sActive  :: a
      , _sParser  :: OptParser a
      }

data Parser a
  = Parser (Optparse.Parser a) [OptError]
  deriving Functor

instance Applicative Parser where
  pure x = Parser (pure x) []

  Parser f e <*> Parser x e' = Parser (f <*> x) (e <> e')

data ParserInfo a
  = ParserInfo (Optparse.ParserInfo a) [OptError]
  deriving Functor

data OptError
  = OptError
      { _oeOpt  :: SomeOpt
      , _oeDesc :: String
      }

data SomeOpt where
  SomeOpt :: Opt a -> SomeOpt

-- Single
newtype Single (b :: Type) (f :: Type -> Type)
  = Single
      { getSingle :: f b
      }

single :: f b -> Single b f
single = Single

deriving instance (Show b, Show (f b)) => Show (Single b f)
deriving newtype instance Generic (f b) => Generic (Single b f)

instance B.FunctorB (Single b) where
  bmap nat (Single p) = Single (nat p)

instance B.ProductB (Single b) where
  bprod (Single l) (Single r) = Single (P.Pair l r)
  buniq = Single

instance B.TraversableB (Single b) where
  btraverse nat (Single p) = Single <$> nat p

-- Nested
newtype Nested (b :: Type) (f :: Type -> Type)
  = Nested (HKD.HKD b f)

type family Nest (a :: Type) (f :: Type -> Type) = (res :: Type) | res -> a where
  Nest (a -> b)      f = a -> Nest b f
  Nest (HKD.HKD a f) f = Nested a f

nested
  :: forall b f k.
     ( HKD.Build b f k
     , Coercible (HKD.HKD b f) (Nested b f)
     , Coercible k (Nest k f)
     )
  => Nest k f
nested = coerce @k @(Nest k f) hkd
  where hkd = HKD.build @b @f @k

getNested
  :: HKD.Construct f b
  => Nested b f
  -> f b
getNested (Nested hkd) = HKD.construct hkd

deriving newtype instance Generic (HKD.HKD b f) => Generic (Nested b f)
deriving newtype instance B.FunctorB (HKD.HKD b) => B.FunctorB (Nested b)
deriving newtype instance B.ProductB (HKD.HKD b) => B.ProductB (Nested b)

instance (B.TraversableB (HKD.HKD b)) => B.TraversableB (Nested b) where
  btraverse nat (Nested hkd) = Nested <$> B.btraverse nat hkd
