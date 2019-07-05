{-# LANGUAGE RankNTypes #-}
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


type OptReader a = String -> Either String a

-- Option
data Opt a
  = Opt
      { _optLong    :: Maybe String
      , _optShort   :: Maybe Char
      , _optHelp    :: Maybe String
      , _optMetavar :: Maybe String
      , _optEnvVar  :: Maybe String
      , _optDefault :: Maybe a
      , _optReader  :: OptReader a
      , _optType    :: OptType a
      }
  deriving Functor

data OptType a
  = OptionOptType
  | FlagOptType a  -- active value
  | ArgumentOptType
  deriving Functor

-- Option for flags with arguments
data OptionOpt a
  = OptionOpt
      { _oLong    :: Maybe String
      , _oShort   :: Maybe Char
      , _oHelp    :: Maybe String
      , _oMetavar :: Maybe String
      , _oEnvVar  :: Maybe String
      , _oDefault :: Maybe a
      , _oReader  :: OptReader a
      }

-- Option for flags that act like switches between a default and an active
-- value
data FlagOpt a
  = FlagOpt
      { _sLong    :: Maybe String
      , _sShort   :: Maybe Char
      , _sHelp    :: Maybe String
      , _sEnvVar  :: Maybe String
      , _sDefault :: a
      , _sActive  :: a
      }

-- Option for arguments (no long/short specifiers)
data ArgumentOpt a
  = ArgumentOpt
      { _aHelp    :: Maybe String
      , _aMetavar :: Maybe String
      , _aEnvVar  :: Maybe String
      , _aDefault :: Maybe a
      , _aReader  :: OptReader a
      }

-- Parser
data Parser a
  = Parser (Optparse.Parser a) [OptError]
  deriving Functor

instance Applicative Parser where
  pure x = Parser (pure x) []

  Parser f e <*> Parser x e' = Parser (f <*> x) (e <> e')

data OptError
  = OptError
      { _oeOpt  :: SomeOpt
      , _oeDesc :: String
      }

instance Eq OptError where
  OptError (SomeOpt opt) desc == OptError (SomeOpt opt') desc'
    =  _optLong opt == _optLong opt'
    && desc == desc'

data SomeOpt where
  SomeOpt :: Opt a -> SomeOpt

toOptError
  :: Opt a
  -> String
  -> OptError
toOptError
  = OptError . SomeOpt

type Environment
  = [(String, String)]

data ParserSource
  = EnvSource Environment
  deriving Eq

data SourceParseResult a
  = SourceNotAvailable
  | OptNotFound
  | OptFoundNoParse OptError
  | OptParsed a

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
