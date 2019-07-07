{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Options.Harg.Operations where

import           System.Environment    (getArgs)
import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Compose  (Compose(..))
import           Data.Functor.Const    (Const(..))

import qualified Data.Barbie           as B
import qualified Options.Applicative   as Optparse

import           Options.Harg.Parser
import           Options.Harg.Pretty
import           Options.Harg.Sources
import           Options.Harg.Types
import           Options.Harg.Util
import           Options.Harg.Cmdline  (mkOptparseParser)


execParserDef
  :: Parser a
  -> Optparse.Parser b
  -> IO (a, b)
execParserDef p extra
  = do
      args <- getArgs
      let (res, errs) = execParserDefPure p args extra
      case res of
        Optparse.Success a
          -> ppWarning errs >> pure a
        _
          -> ppError errs >> Optparse.handleParseResult res

execParserDefPure
  :: Parser a
  -> [String]
  -> Optparse.Parser b
  -> (Optparse.ParserResult (a, b), [OptError])
execParserDefPure (Parser parser err) args extra
  = let
      parserInfo
        = Optparse.info (Optparse.helper <*> ((,) <$> parser <*> extra)) Optparse.forwardOptions
      res
        = Optparse.execParserPure Optparse.defaultPrefs parserInfo args

    in (res, err)

-- getOptparseParser
--   :: GetParser a
--   => a
--   -> IO (Optparse.Parser (OptResult a))
-- getOptparseParser a
--   = do
--       sources <- getSources
--       getOptparseParserPure sources a

-- getOptparseParserPure
--   :: GetParser a
--   => [ParserSource]
--   -> a
--   -> IO (Optparse.Parser (OptResult a))
-- getOptparseParserPure sources a
--   = fst <$> getOptparseParserAndErrorsPure sources a

-- getOptparseParserAndErrors
  -- :: GetParser a
  -- => a
  -- -> IO (Optparse.Parser (OptResult a), [OptError])
-- getOptparseParserAndErrors a
  -- = do
      -- sources <- getSources
      -- pure $ getOptparseParserAndErrorsPure sources a

-- getOptparseParserAndErrorsPure
--   :: GetParser a
--   => [ParserSource]
--   -> a
--   -> IO (Optparse.Parser (OptResult a), [OptError])
-- getOptparseParserAndErrorsPure sources a
--   = do Parser p err <- getParser sources a
--        pure (p, err)

execOpt'
  :: ( B.TraversableB a
     , B.ProductB a
     , B.TraversableB c
     , B.ProductB c
     , GetSources (c Identity) a
     )
  => c Opt
  -> a Opt
  -> IO (a Identity)
execOpt' c a
  = do
      parser <- mkOptparseParser [] (compose Identity c)
      dummyParser <- mkOptparseParser [] (toDummyOpts @String a)
      let allParser = (,) <$> parser <*> dummyParser
      (yes, _notyet)
        <- Optparse.execParser
             (Optparse.info (Optparse.helper <*> allParser) mempty)
      -- sources <- getSources' yes
      (errs, sources) <- getSources' yes a
      p <- getOptParser sources a
      (res, _) <- execParserDef (Parser p errs) parser
      pure res

toDummyOpts
  :: forall m a.
     ( B.FunctorB a
     , Monoid m
     )
  => a Opt
  -> a (Compose Opt (Const m))
toDummyOpts
  = B.bmap toDummy
  where
    toDummy opt@Opt{..}
      = Compose
      $ Const
      <$> opt
            { _optDefault = Just mempty
            , _optReader  = pure . const mempty
            , _optType
                = case _optType of
                    OptionOptType   -> OptionOptType
                    FlagOptType _   -> FlagOptType mempty
                    ArgumentOptType -> ArgumentOptType
            }

-- execOpt
--   :: GetParser a
--   => a
--   -> IO (OptResult a)
-- execOpt a
--   = do
--       sources <- getSources
--       execParserDef =<< getParser sources a

-- execOptPure
--   :: GetParser a
--   => [String]
--   -> [ParserSource]
--   -> a
--   -> IO (Optparse.ParserResult (OptResult a), [OptError])
-- execOptPure args sources a
--   = do
--       p <- getParser sources a
--       pure $ execParserDefPure p args

