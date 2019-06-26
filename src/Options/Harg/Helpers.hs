module Options.Harg.Helpers where

import           Control.Monad           ((<=<))
import qualified System.Environment      as Env
import           Text.Read               (readMaybe)

import qualified Options.Applicative     as Optparse

import           Options.Harg.Operations
import           Options.Harg.Pretty
import           Options.Harg.Types

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

execParserDef
  :: Parser a
  -> IO a
execParserDef (Parser parser err)
  = do
      args <- Env.getArgs

      let
        parserInfo
          = Optparse.info (Optparse.helper <*> parser) mempty
        res
          = Optparse.execParserPure Optparse.defaultPrefs parserInfo args

      case res of
        Optparse.Success a
          -> ppWarning err >> pure a
        _
          -> ppError err >> Optparse.handleParseResult res

getOptparseParser
  :: GetParser a
  => a
  -> IO (Optparse.Parser (OptResult a))
getOptparseParser a
  = do
      Parser p _ <- getParser a
      pure p

execOpt
  :: GetParser a
  => a
  -> IO (OptResult a)
execOpt
  = execParserDef <=< getParser
