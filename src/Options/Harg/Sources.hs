module Options.Harg.Sources where

import           Data.Foldable              (foldr')
import           Data.Functor.Compose       (Compose (..))

import qualified Data.Barbie                as B

import           Options.Harg.Sources.DefaultStr
import           Options.Harg.Sources.Env
import           Options.Harg.Sources.Types
import           Options.Harg.Types


-- | Accumulate all the successful source results and return them,
-- along with a list of errors.
accumSourceResults
  :: forall a f.
     B.TraversableB a
  => [Either SourceRunError (a (Compose SourceRunResult f))]
  -> ([SourceRunError], [a (Compose Maybe f)])
accumSourceResults
  = foldr' accumResult ([], [])
  where
    accumResult
      :: Either SourceRunError (a (Compose SourceRunResult f))
      -> ([SourceRunError], [a (Compose Maybe f)])
      -> ([SourceRunError], [a (Compose Maybe f)])
    accumResult res (e, a)
      = case res of
          Left sre -> ([sre] <> e, a)
          Right res' ->
            case B.btraverse go res' of
              (e', a') -> (e' <> e, a' : a)
    go
      :: Compose SourceRunResult f x
      -> ([SourceRunError], Compose Maybe f x)
    go x
      = case getCompose x of
          OptFoundNoParse (OptError o src desc) -> ([SourceRunError (Just o) src desc], Compose Nothing)
          OptParsed a       -> ([], Compose (Just a))
          _                 -> ([], Compose Nothing)

type HiddenSources = DefaultStrSource

-- | Sources hidden from user that are always enabled
hiddenSources :: HiddenSources f
hiddenSources
  = DefaultStrSource

type DefaultSources = EnvSource

-- | Default sources, equivalent to 'EnvSource'
defaultSources :: DefaultSources f
defaultSources
  = EnvSource
