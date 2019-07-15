{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Sources where

import           Data.Foldable              (foldr')
import           Data.Functor.Compose       (Compose (..))

import qualified Data.Barbie                as B

import           Options.Harg.Sources.Env
import           Options.Harg.Sources.Types
import           Options.Harg.Types


-- | Accumulate all the successful source results and return them,
-- along with a list of errors.
accumSourceResults
  :: forall a f.
     B.TraversableB a
  => [a (Compose SourceRunResult f)]
  -> ([OptError], [a (Compose Maybe f)])
accumSourceResults
  = foldr' accumResult ([], [])
  where
    accumResult
      :: a (Compose SourceRunResult f)
      -> ([OptError], [a (Compose Maybe f)])
      -> ([OptError], [a (Compose Maybe f)])
    accumResult res (e, a)
      = case B.btraverse go res of
          (e', a') -> (e' <> e, a' : a)
    go
      :: Compose SourceRunResult f x
      -> ([OptError], Compose Maybe f x)
    go x
      = case getCompose x of
          OptFoundNoParse e -> ([e], Compose Nothing)
          OptParsed a       -> ([], Compose (Just a))
          _                 -> ([], Compose Nothing)

-- | Default sources, equivalent to 'EnvSource'
defaultSources :: EnvSource f
defaultSources
  = EnvSource
