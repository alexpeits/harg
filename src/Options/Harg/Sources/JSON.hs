module Options.Harg.Sources.JSON where

import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Data.Aeson                 as JSON
import qualified Data.Barbie                as B

import           Options.Harg.Types

import Debug.Trace (trace)

getJSON
  :: String
  -> IO (Maybe JSON.Value)
getJSON path
  = do
      contents <- LBS.readFile path
      pure $ JSON.decode contents

runJSONSource
  :: forall a.
     ( B.FunctorB a
     , JSON.FromJSON (a Maybe)
     )
  => JSON.Value
  -> a Opt
  -> a SourceParseResult
runJSONSource json opt
  = let
      res :: JSON.Result (a Maybe)
      res
        = JSON.fromJSON json
      toSuccess :: Maybe x -> SourceParseResult x
      toSuccess
        = maybe OptNotFound OptParsed
      toFailure :: Opt x -> SourceParseResult x
      toFailure
        = const OptNotFound
    in case res of
         JSON.Success v -> B.bmap toSuccess v
         JSON.Error e -> trace (show e) $ B.bmap toFailure opt

-- runEnvVarSource
--   :: B.FunctorB a
--   => Environment
--   -> a Opt
--   -> a SourceParseResult
-- runEnvVarSource env
--   = B.bmap go
--   where
--     go :: Opt a -> SourceParseResult a
--     go opt@Opt{..}
--       = case _optEnvVar of
--           Nothing
--             -> SourceNotAvailable
--           Just envVar
--             -> maybe OptNotFound tryParse $ lookupEnv env envVar
--       where
--         tryParse
--           = either
--               (OptFoundNoParse . toOptError opt)
--               OptParsed
--           . _optReader
