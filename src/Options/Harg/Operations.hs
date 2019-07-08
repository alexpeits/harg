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
  :: Optparse.Parser a
  -> [OptError]
  -> IO a
execParserDef parser errs
  = do
      args <- getArgs
      let res = execParserDefPure args parser
      case res of
        Optparse.Success a
          -> ppWarning errs >> pure a
        _
          -> ppError errs >> Optparse.handleParseResult res

execParserDefPure
  :: [String]
  -> Optparse.Parser a
  -> Optparse.ParserResult a
execParserDefPure args parser
  = let
      parserInfo
        = Optparse.info (Optparse.helper <*> parser) Optparse.forwardOptions
    in Optparse.execParserPure Optparse.defaultPrefs parserInfo args

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
execOpt c opts
  = do
      let
        configParser = mkOptparseParser [] (compose Identity c)
        dummyParser = mkOptparseParser [] (toDummyOpts @String opts)
        allParser = (,) <$> configParser <*> dummyParser
      (config, _) <- Optparse.execParser
                       (Optparse.info (Optparse.helper <*> allParser) mempty)
      sourceVals <- getSource config
      let
        (errs, sources) = accumSourceResults $ runSource sourceVals (compose Identity opts)
        parser = mkOptparseParser sources (compose Identity opts)
      (res, _) <- execParserDef ((,) <$> parser <*> configParser) errs
      pure res

execCommands
  :: forall c ts xs.
     ( B.TraversableB (VariantF xs)
     , B.TraversableB c
     , B.ProductB c
     , Subcommands Z ts xs '[]
     , GetSource c Identity
     , All (RunSource (SourceVal c)) xs
     , All (RunSource '[]) xs
     , MapAssocList xs
     )
  => c Opt
  -> AssocListF ts xs Opt
  -> IO (VariantF xs Identity)
execCommands c opts
  = do
      let
        configParser = mkOptparseParser [] (compose Identity c)
        dummyCommands = mapSubcommand @Z @ts @xs @'[] SZ HNil (allToDummyOpts @String opts)
        dummyParser = Optparse.subparser (mconcat dummyCommands)
        allParser = (,) <$> configParser <*> dummyParser
      (config, _) <- Optparse.execParser
                       (Optparse.info (Optparse.helper <*> allParser) mempty)
      sourceVals <- getSource config
      let
        commands = mapSubcommand @Z @ts @xs @'[] SZ sourceVals (mapAssocList (compose Identity) opts)
        parser = Optparse.subparser (mconcat commands)
        errs = []
      (res, _) <- execParserDef ((,) <$> parser <*> configParser) errs
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

