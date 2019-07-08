{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Operations where

import           Data.Functor.Compose     (Compose(..))
import           Data.Functor.Const       (Const(..))
import           Data.Functor.Identity    (Identity(..))
import           System.Environment       (getArgs)

import qualified Data.Barbie              as B
import qualified Options.Applicative      as Optparse

import           Options.Harg.Cmdline     (mkOptparseParser)
import           Options.Harg.Het.All
import           Options.Harg.Het.HList
import           Options.Harg.Het.Nat
import           Options.Harg.Het.Variant
import           Options.Harg.Parser
import           Options.Harg.Pretty
import           Options.Harg.Sources
import           Options.Harg.Types
import           Options.Harg.Util


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

execOpt
  :: forall c a.
     ( B.TraversableB a
     , B.ProductB a
     , B.TraversableB c
     , B.ProductB c
     , GetSource c Identity
     , RunSource (SourceVal c) a
     )
  => c Opt
  -> a Opt
  -> IO (a Identity)
execOpt c a
  = do
      let
        configParser = mkOptparseParser [] (compose Identity c)
        dummyParser = mkOptparseParser [] (toDummyOpts @String a)
        allParser = (,) <$> configParser <*> dummyParser
      (config, _)
        <- Optparse.execParser
             (Optparse.info (Optparse.helper <*> allParser) mempty)
      sourceVals <- getSource config
      let
        (errs, sources) = accumSourceResults $ runSource' sourceVals (compose Identity a)
        parser = mkOptparseParser sources (compose Identity a)
      (res, _) <- execParserDef (Parser parser errs) configParser
      pure res

execOptSubcommand
  :: forall c ts xs.
     ( B.TraversableB (VariantF xs)
     , B.TraversableB c
     , B.ProductB c
     , GetSource c Identity
     , All (RunSource (SourceVal c)) xs
     , All (RunSource '[]) xs
     , Subcommands Z ts xs '[]
     , MapAssocList xs
     )
  => c Opt
  -> AssocListF ts xs Opt
  -> IO (VariantF xs Identity)
execOptSubcommand c a = do
  let
    configParser = mkOptparseParser [] (compose Identity c)
    dummyCommands = mapSubcommand @Z @ts @xs @'[] SZ HNil (allToDummyOpts @String a)
    dummyParser = Optparse.subparser (mconcat dummyCommands)
    allParser = (,) <$> configParser <*> dummyParser
  (config, _)
    <- Optparse.execParser
         (Optparse.info (Optparse.helper <*> allParser) mempty)
  sourceVals <- getSource config
  let
    commands = mapSubcommand @Z @ts @xs @'[] SZ sourceVals (mapAssocList (compose Identity) a)
    parser = Optparse.subparser (mconcat commands)
  (res, _) <- execParserDef (Parser parser []) configParser
  pure res

allToDummyOpts
  :: forall m ts xs.
     ( Monoid m
     , MapAssocList xs
     )
  => AssocListF ts xs Opt
  -> AssocListF ts xs (Compose Opt (Const m))
allToDummyOpts
  = mapAssocList toDummyOpts

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

