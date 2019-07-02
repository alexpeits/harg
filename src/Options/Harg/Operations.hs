module Options.Harg.Operations where

import           System.Environment  (getArgs, getEnvironment)

import qualified Options.Applicative as Optparse

import           Options.Harg.Env
import           Options.Harg.Parser
import           Options.Harg.Pretty
import           Options.Harg.Types


execParserDef
  :: Parser a
  -> IO a
execParserDef p
  = do
      args <- getArgs
      let (res, errs) = execParserDefPure p args
      case res of
        Optparse.Success a
          -> ppWarning errs >> pure a
        _
          -> ppError errs >> Optparse.handleParseResult res

execParserDefPure
  :: Parser a
  -> [String]
  -> (Optparse.ParserResult a, [OptError])
execParserDefPure (Parser parser err) args
  = let
      parserInfo
        = Optparse.info (Optparse.helper <*> parser) mempty
      res
        = Optparse.execParserPure Optparse.defaultPrefs parserInfo args

    in (res, err)

getOptparseParser
  :: GetParser a
  => a
  -> IO (Optparse.Parser (OptResult a))
getOptparseParser a
  = do
      env <- getEnvironment
      pure $ getOptparseParserPure env a

getOptparseParserPure
  :: GetParser a
  => Environment
  -> a
  -> Optparse.Parser (OptResult a)
getOptparseParserPure env a
  = fst $ getOptparseParserAndErrorsPure env a

getOptparseParserAndErrors
  :: GetParser a
  => a
  -> IO (Optparse.Parser (OptResult a), [OptError])
getOptparseParserAndErrors a
  = do
      env <- getEnvironment
      pure $ getOptparseParserAndErrorsPure env a

getOptparseParserAndErrorsPure
  :: GetParser a
  => Environment
  -> a
  -> (Optparse.Parser (OptResult a), [OptError])
getOptparseParserAndErrorsPure env a
  = let Parser p err = getParser env a
    in (p, err)

execOpt
  :: GetParser a
  => a
  -> IO (OptResult a)
execOpt a
  = do
      env <- getEnvironment
      execParserDef (getParser env a)

execOptPure
  :: GetParser a
  => a
  -> [String]
  -> Environment
  -> (Optparse.ParserResult (OptResult a), [OptError])
execOptPure a args env
  = execParserDefPure (getParser env a) args

