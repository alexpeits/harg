{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Operations where

import           Data.Functor.Identity      (Identity(..))

import qualified Data.Barbie                as B
import qualified Options.Applicative        as Optparse

import           Options.Harg.Cmdline       (mkOptparseParser)
import           Options.Harg.Het.All
import           Options.Harg.Het.HList
import           Options.Harg.Het.Variant
import           Options.Harg.Pretty
import           Options.Harg.Sources
import           Options.Harg.Sources.Env
import           Options.Harg.Sources.Types
import           Options.Harg.Subcommands
import           Options.Harg.Types
import           Options.Harg.Util
import           Options.Harg.Config


execOptWithCtx
  :: forall c a.
     ( B.TraversableB a
     , B.ProductB a
     , B.TraversableB c
     , B.ProductB c
     , GetSource c Identity
     , RunSource (SourceVal c) a
     )
  => HargCtx
  -> c Opt
  -> a Opt
  -> IO (a Identity)
execOptWithCtx ctx conf opts
  = do
      let
        configParser = mkConfigParser ctx (compose Identity conf)
        dummyParser = mkOptparseParser [] (toDummyOpts @String opts)
      config <- getConfig ctx configParser dummyParser
      sourceVals <- getSource ctx config
      let
        (errs, sources)
          = accumSourceResults
          $ runSource sourceVals (compose Identity opts)
        parser
          = mkOptparseParser sources (compose Identity opts)
      (res, _) <- execParser ctx ((,) <$> parser <*> configParser) errs
      pure res

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
execOpt conf opts
  = do
      ctx <- getCtx
      execOptWithCtx ctx conf opts

execOptWithCtxDef
  :: forall a.
     ( B.TraversableB a
     , B.ProductB a
     )
  => HargCtx
  -> a Opt
  -> IO (a Identity)
execOptWithCtxDef ctx
  = execOptWithCtx ctx defaultSources

execOptDef
  :: forall a.
     ( B.TraversableB a
     , B.ProductB a
     )
  => a Opt
  -> IO (a Identity)
execOptDef
  = execOpt defaultSources

execCommandsWithCtx
  :: forall c ts xs.
     ( B.TraversableB (VariantF xs)
     , B.TraversableB c
     , B.ProductB c
     , Subcommands ts xs
     , GetSource c Identity
     , All (RunSource (SourceVal c)) xs
     , All (RunSource ()) xs
     , MapAssocList xs
     )
  => HargCtx
  -> c Opt
  -> AssocListF ts xs Opt
  -> IO (VariantF xs Identity)
execCommandsWithCtx ctx conf opts
  = do
      let
        configParser = mkConfigParser ctx (compose Identity conf)
        (_, dummyCommands)
          = mapSubcommand () (allToDummyOpts @String opts)
        dummyParser
          = Optparse.subparser (mconcat dummyCommands)

      config <- getConfig ctx configParser dummyParser
      sourceVals <- getSource ctx config

      let
        (errs, commands)
          = mapSubcommand sourceVals (mapAssocList (compose Identity) opts)
        parser
          = Optparse.subparser (mconcat commands)
      (res, _) <- execParser ctx ((,) <$> parser <*> configParser) errs
      pure res

execCommands
  :: forall c ts xs.
     ( B.TraversableB (VariantF xs)
     , B.TraversableB c
     , B.ProductB c
     , Subcommands ts xs
     , GetSource c Identity
     , All (RunSource (SourceVal c)) xs
     , All (RunSource ()) xs
     , MapAssocList xs
     )
  => c Opt
  -> AssocListF ts xs Opt
  -> IO (VariantF xs Identity)
execCommands conf opts
  = do
      ctx <- getCtx
      execCommandsWithCtx ctx conf opts

execCommandsWithCtxDef
  :: forall ts xs.
     ( B.TraversableB (VariantF xs)
     , Subcommands ts xs
     , All (RunSource EnvSourceVal) xs
     , All (RunSource ()) xs
     , MapAssocList xs
     )
  => HargCtx
  -> AssocListF ts xs Opt
  -> IO (VariantF xs Identity)
execCommandsWithCtxDef ctx
  = execCommandsWithCtx ctx defaultSources

execCommandsDef
  :: forall ts xs.
     ( B.TraversableB (VariantF xs)
     , Subcommands ts xs
     , All (RunSource EnvSourceVal) xs
     , All (RunSource ()) xs
     , MapAssocList xs
     )
  => AssocListF ts xs Opt
  -> IO (VariantF xs Identity)
execCommandsDef
  = execCommands defaultSources

execParser
  :: HargCtx
  -> Optparse.Parser a
  -> [OptError]
  -> IO a
execParser HargCtx{..} parser errs
  = do
      let
        res = execParserPure _hcArgs parser
      case res of
        Optparse.Success a
          -> ppWarning errs >> pure a
        _
          -> ppError errs >> Optparse.handleParseResult res

execParserPure
  :: [String]
  -> Optparse.Parser a
  -> Optparse.ParserResult a
execParserPure args parser
  = let
      parserInfo
        = Optparse.info (Optparse.helper <*> parser) Optparse.forwardOptions
    in Optparse.execParserPure Optparse.defaultPrefs parserInfo args
