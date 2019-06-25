module Data.Harg.Options.Helpers where

import           System.Exit                  (exitFailure)
import           Text.Read                    (readMaybe)

import qualified Data.Validation              as V

import           Data.Harg.Options.Pretty
import           Data.Harg.Options.Types

optValue
  :: (OptError -> r)
  -> (a -> r)
  -> OptValue a
  -> r
optValue e f (OptValue a)
  = V.validation e f a

execOpt
  :: OptValue a
  -> IO a
execOpt
  = optValue
      ((>> exitFailure) . putStrLn . ppError)
      pure

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
