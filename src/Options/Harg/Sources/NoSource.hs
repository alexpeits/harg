{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module Options.Harg.Sources.NoSource
  ( NoSource
  , noSources
  ) where

import           Data.Kind                  (Type)
import           GHC.Generics               (Generic)

import qualified Data.Barbie                as B

import           Options.Harg.Sources.Types


-- | Throwaway type whose 'GetSource' instance returns no value.
data NoSource (f :: Type -> Type) = NoSource
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

instance GetSource NoSource f where
  type SourceVal NoSource = ()
  getSource _ctx _ = pure ()

-- | Shorthand for writing 'NoSource'.
noSources :: NoSource f
noSources
  = NoSource
