module Options.Harg.Helpers where

import           Data.Functor.Identity (Identity(..))
import           Text.Read             (readMaybe)

import           Options.Harg.Types

extractOpt :: Identity a -> a
extractOpt
  = runIdentity

parseWith
  :: (String -> Maybe a)
  -> String
  -> Either String a
parseWith parser s
  = maybe (Left err) Right (parser s)
  where
    err
      = "Unable to parse: " <> s

readParser :: Read a => OptParser a
readParser
  = parseWith readMaybe

strParser :: String -> Either String String
strParser
  = pure
