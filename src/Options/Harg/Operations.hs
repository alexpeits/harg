module Options.Harg.Operations where

import           System.Environment   (getArgs)

import qualified Options.Applicative  as Optparse

import           Options.Harg.Parser
import           Options.Harg.Pretty
import           Options.Harg.Sources
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
        = Optparse.info (Optparse.helper <*> parser) Optparse.forwardOptions
      res
        = Optparse.execParserPure Optparse.defaultPrefs parserInfo args

    in (res, err)

getOptparseParser
  :: GetParser a
  => a
  -> IO (Optparse.Parser (OptResult a))
getOptparseParser a
  = do
      sources <- getSources
      getOptparseParserPure sources a

getOptparseParserPure
  :: GetParser a
  => [ParserSource]
  -> a
  -> IO (Optparse.Parser (OptResult a))
getOptparseParserPure sources a
  = fst <$> getOptparseParserAndErrorsPure sources a

-- getOptparseParserAndErrors
  -- :: GetParser a
  -- => a
  -- -> IO (Optparse.Parser (OptResult a), [OptError])
-- getOptparseParserAndErrors a
  -- = do
      -- sources <- getSources
      -- pure $ getOptparseParserAndErrorsPure sources a

getOptparseParserAndErrorsPure
  :: GetParser a
  => [ParserSource]
  -> a
  -> IO (Optparse.Parser (OptResult a), [OptError])
getOptparseParserAndErrorsPure sources a
  = do Parser p err <- getParser sources a
       pure (p, err)

execOpt
  :: GetParser a
  => a
  -> IO (OptResult a)
execOpt a
  = do
      sources <- getSources
      execParserDef =<< getParser sources a

execOptPure
  :: GetParser a
  => [String]
  -> [ParserSource]
  -> a
  -> IO (Optparse.ParserResult (OptResult a), [OptError])
execOptPure args sources a
  = do
      p <- getParser sources a
      pure $ execParserDefPure p args

