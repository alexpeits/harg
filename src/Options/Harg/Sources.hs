module Options.Harg.Sources where

import           Data.Foldable            (foldr')
import           System.Environment       (getEnvironment)

import qualified Data.Barbie              as B

import           Options.Harg.Sources.Env
import           Options.Harg.Types


runSources
  :: B.FunctorB a
  => [ParserSource]
  -> a Opt
  -> [a SourceParseResult]
runSources sources opt
  = map (`runSource` opt) sources

runSource
  :: B.FunctorB a
  => ParserSource
  -> a Opt
  -> a SourceParseResult
runSource source opt
  = case source of
      EnvSource env
        -> runEnvVarSource env opt

accumSourceResults
  :: forall a. B.TraversableB a
  => [a SourceParseResult]
  -> ([OptError], [a Maybe])
accumSourceResults
  = foldr' accumResult ([], [])
  where
    accumResult res (e, a)
      = case B.btraverse go res of
          (e', a') -> (e' <> e, a' : a)
    go
      = \case
          OptFoundNoParse e -> ([e], Nothing)
          OptParsed a       -> ([], Just a)
          _                 -> ([], Nothing)

-- Currently only environment
getSources
  :: IO [ParserSource]
getSources
  = pure . EnvSource <$> getEnvironment
