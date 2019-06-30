module Options.Harg.Operations where

import           System.Environment  (getArgs)

import qualified Options.Applicative as Optparse

import           Options.Harg.Parser
import           Options.Harg.Pretty
import           Options.Harg.Types


execParserDef
  :: Parser a
  -> IO a
execParserDef p
  = getArgs >>= execParserPureDef p

execParserPureDef
  :: Parser a
  -> [String]
  -> IO a
execParserPureDef (Parser parser err) args
  = do
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
  = fst <$> getOptparseParserAndErrors a

getOptparseParserAndErrors
  :: GetParser a
  => a
  -> IO (Optparse.Parser (OptResult a), [OptError])
getOptparseParserAndErrors a
  = do
      Parser p err <- getParser a
      pure (p, err)

execOpt
  :: GetParser a
  => a
  -> IO (OptResult a)
execOpt a
  = getArgs >>= execOptPure a

execOptPure
  :: GetParser a
  => a
  -> [String]
  -> IO (OptResult a)
execOptPure a args
  = getParser a >>= flip execParserPureDef args
