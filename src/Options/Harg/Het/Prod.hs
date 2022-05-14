{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Options.Harg.Het.Prod
  ( (:*) (..),
    Tagged (..),
  )
where

import qualified Barbies as B
import Data.Aeson ((.!=), (.:?))
import qualified Data.Aeson as JSON
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as JSON.Key
#endif
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Tx
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Infix version of 'Data.Functor.Product'. Allows to combine
-- higher-kinded types, and keep them partially applied until needed:
--
-- @
--   data User = User { name :: String, age :: Int }
--     deriving Generic
--
--   type Config = Nested User :* Single Int
--
--   configOpt :: Config Opt
--   configOpt = ...
-- @
data
  ( (a :: (Type -> Type) -> Type)
      :* (b :: (Type -> Type) -> Type)
  )
    (f :: Type -> Type)
  = a f :* b f
  deriving (Generic, B.FunctorB, B.TraversableB, B.ApplicativeB)

infixr 4 :*

deriving instance
  ( Show (a Identity),
    Show (b Identity)
  ) =>
  Show ((a :* b) Identity)

-- | This type adds a type-level phantom tag to a higher-kinded type.
-- Its JSON instance allows using ':*' with 'Options.Harg.Sources.JSON.JSONSource'.
newtype
  Tagged
    (t :: k)
    (a :: (Type -> Type) -> Type)
    (f :: Type -> Type) = Tagged
  { unTagged :: a f
  }
  deriving (Generic)

deriving newtype instance JSON.FromJSON (a f) => JSON.FromJSON (Tagged t a f)

instance B.FunctorB a => B.FunctorB (Tagged t a) where
  bmap nat (Tagged x) = Tagged (B.bmap nat x)

instance B.TraversableB a => B.TraversableB (Tagged t a) where
  btraverse nat (Tagged x) = Tagged <$> B.btraverse nat x

instance B.ApplicativeB a => B.ApplicativeB (Tagged t a) where
  bprod (Tagged l) (Tagged r) = Tagged (B.bprod l r)
  bpure f = Tagged (B.bpure f)

-- The following JSON instances need to work if and only if all elements in
-- the product are `Tagged`, hence the weird pattern matches
instance
  ( JSON.FromJSON (a Maybe),
    JSON.FromJSON (b' Maybe),
    B.ApplicativeB a,
    B.ApplicativeB b',
    KnownSymbol ta,
    b' ~ (Tagged tb b :* c)
  ) =>
  JSON.FromJSON ((Tagged ta a :* (Tagged tb b :* c)) Maybe)
  where
  parseJSON =
    JSON.withObject ":*" $
      \o ->
        (:*)
          <$> o .:? symbolToKey (Proxy @ta) .!= B.bpure Nothing
          <*> JSON.parseJSON (JSON.Object o)

instance
  ( JSON.FromJSON (a Maybe),
    JSON.FromJSON (b Maybe),
    B.ApplicativeB a,
    B.ApplicativeB b,
    KnownSymbol ta,
    KnownSymbol tb
  ) =>
  JSON.FromJSON ((Tagged ta a :* Tagged tb b) Maybe)
  where
  parseJSON =
    JSON.withObject ":*" $
      \o ->
        (:*)
          <$> o .:? symbolToKey (Proxy @ta) .!= B.bpure Nothing
          <*> o .:? symbolToKey (Proxy @tb) .!= B.bpure Nothing

#if MIN_VERSION_aeson(2,0,0)
symbolToKey :: forall (k :: Symbol). KnownSymbol k => Proxy k -> JSON.Key
symbolToKey p =
  JSON.Key.fromText (Tx.pack (symbolVal p))
#else
symbolToKey :: forall (k :: Symbol). KnownSymbol k => Proxy k -> Tx.Text
symbolToKey p =
  Tx.pack (symbolVal p)
#endif
