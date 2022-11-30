{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Options.Harg.Single
  ( Single (..),
    single,
    fromSingle,
  )
where

import qualified Barbies as B
import qualified Data.Aeson as JSON
import Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Product as P
import Data.Kind (Type)
import GHC.Generics (Generic)

-- | @Single a f@ is a newtype around @f a@, which allows mixing non-nested
-- with nested values when creating configuration parsers, using
-- 'Options.Harg.Het.Prod.:*'.
--
-- @
--   data User = User { name :: String, age :: Int }
--     deriving Generic
--
--   myConfig :: (Nested User :* Single Int) Opt
--   myConfig
--     =  nested @User nameOpt ageOpt
--     :* single intOpt
--     where
--       ...
-- @
newtype Single (a :: Type) (f :: Type -> Type) = Single
  { getSingle :: f a
  }

-- | Wrap a value into a 'Single'.
single :: f a -> Single a f
single = Single

-- | Helper for when f ~ Identity
fromSingle :: Single a Identity -> a
fromSingle = runIdentity . getSingle

deriving instance (Show a, Show (f a)) => Show (Single a f)

deriving newtype instance Generic (f a) => Generic (Single a f)

deriving newtype instance JSON.FromJSON (f a) => JSON.FromJSON (Single a f)

instance B.FunctorB (Single a) where
  bmap nat (Single p) = Single (nat p)

instance B.TraversableB (Single a) where
  btraverse nat (Single p) = Single <$> nat p

instance B.ApplicativeB (Single a) where
  bprod (Single l) (Single r) = Single (P.Pair l r)
  bpure x = Single x
