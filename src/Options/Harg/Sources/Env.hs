module Options.Harg.Sources.Env where

import           Data.Functor.Compose (Compose (..))
import           Data.List            (find)

import qualified Data.Barbie          as B

import           Options.Harg.Types


data EnvVarParseResult a
  = EnvVarNotAvailable
  | EnvVarNotFound
  | EnvVarFoundNoParse String
  | EnvVarParsed a

lookupEnv
  :: Environment
  -> String
  -> Maybe String
lookupEnv env x
  = snd <$> find ((== x) . fst) env

getEnvVar :: EnvVarParseResult a -> Maybe a
getEnvVar
  = \case
      EnvVarParsed a -> Just a
      _              -> Nothing

tryParseEnvVar
  :: Environment
  -> Opt a
  -> EnvVarParseResult a
tryParseEnvVar env Opt{..}
  = case _optEnvVar of
      Nothing
        -> EnvVarNotAvailable
      Just envVar
        -> maybe EnvVarNotFound tryParse $ lookupEnv env envVar
  where
    tryParse
      = either EnvVarFoundNoParse EnvVarParsed . _optReader

runEnvVarSource
  :: forall a f.
     ( B.FunctorB a
     , Applicative f
     )
  => Environment
  -> a (Compose Opt f)
  -> a (Compose SourceParseResult f)
runEnvVarSource env
  = B.bmap go
  where
    go :: Compose Opt f x -> Compose SourceParseResult f x
    go (Compose opt@Opt{..})
      = case _optEnvVar of
          Nothing
            -> Compose $ pure <$> SourceNotAvailable
          Just envVar
            -> Compose $ maybe OptNotFound tryParse (lookupEnv env envVar)
      where
        tryParse
          = either
              (OptFoundNoParse . toOptError opt)
              OptParsed
          . _optReader
