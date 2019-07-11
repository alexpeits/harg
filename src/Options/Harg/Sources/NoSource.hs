{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module Options.Harg.Sources.NoSource where

import           Data.Kind                  (Type)
import           GHC.Generics               (Generic)

import qualified Data.Barbie                as B

import           Options.Harg.Sources.Types


data NoSource (f :: Type -> Type) = NoSource
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

instance GetSource NoSource f where
  type SourceVal NoSource = ()
  getSource _ctx _ = pure ()

noSources :: NoSource f
noSources
  = NoSource
