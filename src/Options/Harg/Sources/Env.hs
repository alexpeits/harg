module Options.Harg.Sources.Env where

import           Data.Functor.Compose (Compose (..))
import           Data.List            (find)

import qualified Data.Barbie          as B

import           Options.Harg.Types


lookupEnv
  :: Environment
  -> String
  -> Maybe String
lookupEnv env x
  = snd <$> find ((== x) . fst) env

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
