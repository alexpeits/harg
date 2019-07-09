{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
module Options.Harg.Het.Prod where

import           Data.Functor.Identity (Identity)
import           Data.Kind             (Type)
import           Data.Proxy            (Proxy(..))
import           GHC.Generics          (Generic)
import           GHC.TypeLits          (KnownSymbol, symbolVal)

import           Data.Aeson            ((.:?), (.!=))
import qualified Data.Aeson            as JSON
import qualified Data.Barbie           as B
import qualified Data.Text             as Tx


newtype Tagged
    (t :: k)
    (a :: (Type -> Type) -> Type)
    (f :: Type -> Type)
  = Tagged
      { unTagged :: a f
      }
  deriving (Generic)

deriving newtype instance JSON.FromJSON (a f) => JSON.FromJSON (Tagged t a f)

instance B.FunctorB a => B.FunctorB (Tagged t a) where
  bmap nat (Tagged x) = Tagged (B.bmap nat x)

instance B.TraversableB a => B.TraversableB (Tagged t a) where
  btraverse nat (Tagged x) = Tagged <$> B.btraverse nat x

instance B.ProductB a => B.ProductB (Tagged t a) where
  bprod (Tagged l) (Tagged r) = Tagged (B.bprod l r)
  buniq f = Tagged (B.buniq f)

data
    ((a :: (Type -> Type) -> Type) :* (b :: (Type -> Type) -> Type))
    (f :: Type -> Type)
  = a f :* b f
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

infixr 4 :*

deriving instance
  ( Show (a Identity)
  , Show (b Identity)
  ) => Show ((a :* b) Identity)

instance ( JSON.FromJSON (a Maybe)
         , JSON.FromJSON (b' Maybe)
         , B.ProductB a, B.ProductB b'
         , KnownSymbol ta
         , b' ~ (Tagged tb b :* c)
         ) => JSON.FromJSON ((Tagged ta a :* (Tagged tb b :* c)) Maybe) where
  parseJSON
    = JSON.withObject ":*"
    $ \o ->
          (:*)
          <$> o .:? Tx.pack (symbolVal (Proxy :: Proxy ta)) .!= B.buniq Nothing
          <*> JSON.parseJSON (JSON.Object o)

instance {-# OVERLAPS #-}
    ( JSON.FromJSON (a Maybe)
    , JSON.FromJSON (b Maybe)
    , B.ProductB a, B.ProductB b
    , KnownSymbol ta
    , KnownSymbol tb
    ) => JSON.FromJSON ((Tagged ta a :* Tagged tb b) Maybe) where
  parseJSON
    = JSON.withObject ":*"
    $ \o ->
          (:*)
          <$> o .:? Tx.pack (symbolVal (Proxy :: Proxy ta)) .!= B.buniq Nothing
          <*> o .:? Tx.pack (symbolVal (Proxy :: Proxy tb)) .!= B.buniq Nothing
