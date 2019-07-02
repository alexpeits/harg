module Options.Harg.Sources where

import Data.Foldable      (foldr')
import System.Environment (getEnvironment)

import Options.Harg.Sources.Env
import Options.Harg.Types


runSources
  :: [ParserSource]
  -> Opt a
  -> [SourceParseResult a]
runSources sources opt
  = map (`runSource` opt) sources

runSource
  :: ParserSource
  -> Opt a
  -> SourceParseResult a
runSource source opt
  = case source of
      EnvSource env
        -> runEnvVarSource env opt

accumSourceResults
  :: [SourceParseResult a]
  -> ([Maybe a], [OptError])
accumSourceResults
  = foldr' go ([], [])
  where
    go res (as, errs)
      = case res of
          OptFoundNoParse err -> (as, err:errs)
          OptParsed a         -> (Just a : as, errs)
          _                   -> (Nothing : as, errs)

-- Currently only environment
getSources
  :: IO [ParserSource]
getSources
  = pure . EnvSource <$> getEnvironment
