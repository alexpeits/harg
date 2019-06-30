module Options.Harg.Env where

import           System.Environment         (lookupEnv)

import           Options.Harg.Types

data EnvVarParseResult a
  = EnvVarNotAvailable
  | EnvVarNotFound
  | EnvVarFoundNoParse String
  | EnvVarParsed a

getEnvVar :: EnvVarParseResult a -> Maybe a
getEnvVar
  = \case
      EnvVarParsed a -> Just a
      _              -> Nothing

tryParseEnvVar
  :: Opt a
  -> IO (EnvVarParseResult a)
tryParseEnvVar Opt{..}
  = case _optEnvVar of
      Nothing
        -> pure EnvVarNotAvailable
      Just envVar
        -> maybe EnvVarNotFound tryParse <$> lookupEnv envVar
  where
    tryParse
      = either EnvVarFoundNoParse EnvVarParsed . _optReader
