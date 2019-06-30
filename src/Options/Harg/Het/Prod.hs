{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
module Options.Harg.Het.Prod where

import           Data.Functor.Identity (Identity)
import           Data.Kind             (Type)
import           GHC.Generics          (Generic)

import qualified Data.Barbie           as B

data
    ((a :: (Type -> Type) -> Type) :* (b :: (Type -> Type) -> Type))
    (f :: Type -> Type)
  = a f :* b f
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

deriving instance
  ( Show (a Identity)
  , Show (b Identity)
  ) => Show ((a :* b) Identity)

infixr 4 :*
