{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Options.Harg.Operations
  ( execOpt,
    execOptDef,
    execOptWithCtx,
    execOptWithCtxDef,
    execCommands,
    execCommandsDef,
    execCommandsWithCtx,
    execCommandsWithCtxDef,
  )
where

import qualified Barbies as B
import Data.Functor.Identity (Identity (..))
import qualified Options.Applicative as Optparse
import Options.Harg.Cmdline (mkOptparseParser)
import Options.Harg.Config (getConfig, mkConfigParser)
import Options.Harg.Het.All (All)
import Options.Harg.Het.HList (AssocListF, MapAssocList (..))
import Options.Harg.Het.Prod ((:*) (..))
import Options.Harg.Het.Variant (VariantF)
import Options.Harg.Pretty (ppSourceRunErrors)
import Options.Harg.Sources
  ( DefaultSources,
    HiddenSources,
    accumSourceResults,
    defaultSources,
    hiddenSources,
  )
import Options.Harg.Sources.Types (GetSource (..), RunSource (..), SourceRunError)
import Options.Harg.Subcommands (Subcommands (..))
import Options.Harg.Types (HargCtx (..), Opt, getCtx)
import Options.Harg.Util (allToDummyOpts, compose, toDummyOpts)

-- | Run the option parser and combine with values from the specified sources,
-- passing the context explicitly.
execOptWithCtx ::
  forall c a.
  ( B.TraversableB a,
    B.ApplicativeB a,
    B.TraversableB c,
    B.ApplicativeB c,
    GetSource c Identity,
    RunSource (SourceVal c) a
  ) =>
  -- | Context containing the environment and the cmdline args
  HargCtx ->
  -- | Source options
  c Opt ->
  -- | Target configuration options
  a Opt ->
  IO (a Identity)
execOptWithCtx ctx conf opts = do
  let configParser =
        mkConfigParser ctx $ compose Identity (conf :* hiddenSources)
      dummyParser =
        mkOptparseParser [] (toDummyOpts @String opts)
  config <- getConfig ctx configParser dummyParser
  sourceVals <- getSource ctx config
  let (errs, sources) =
        accumSourceResults $
          runSource sourceVals (compose Identity opts)
      optParser =
        mkOptparseParser sources (compose Identity opts)
      -- parser that includes the configuration options, otherwise parsing
      -- will find more options and fail
      allParser =
        (,) <$> optParser <*> configParser
  fst
    <$> if null errs
      then execParser ctx allParser
      else failParser allParser errs

-- | Run the option parser and combine with values from the specified sources
execOpt ::
  forall c a.
  ( B.TraversableB a,
    B.ApplicativeB a,
    B.TraversableB c,
    B.ApplicativeB c,
    GetSource c Identity,
    RunSource (SourceVal c) a
  ) =>
  -- | Source options
  c Opt ->
  -- | Target configuration options
  a Opt ->
  IO (a Identity)
execOpt conf opts = do
  ctx <- getCtx
  execOptWithCtx ctx conf opts

-- | Run the option parser only with default sources (environment variables),
-- passing the context explicitly.
execOptWithCtxDef ::
  forall a.
  ( B.TraversableB a,
    B.ApplicativeB a
  ) =>
  -- | Context containing the environment and the cmdline args
  HargCtx ->
  -- | Target configuration options
  a Opt ->
  IO (a Identity)
execOptWithCtxDef ctx =
  execOptWithCtx ctx defaultSources

-- | Run the option parser only with default sources (environment variables)
execOptDef ::
  forall a.
  ( B.TraversableB a,
    B.ApplicativeB a
  ) =>
  -- | Target configuration options
  a Opt ->
  IO (a Identity)
execOptDef =
  execOpt defaultSources

-- | Run the subcommand parser and combine with values from the specified
-- sources, passing the context explicitly.
execCommandsWithCtx ::
  forall c ts xs.
  ( B.TraversableB (VariantF xs),
    B.TraversableB c,
    B.ApplicativeB c,
    Subcommands ts xs,
    GetSource c Identity,
    All (RunSource (SourceVal (c :* HiddenSources))) xs,
    All (RunSource ()) xs,
    MapAssocList xs
  ) =>
  -- | Context containing the environment and the cmdline args
  HargCtx ->
  -- | Source options
  c Opt ->
  -- | Target options associated with subcommands
  AssocListF ts xs Opt ->
  IO (VariantF xs Identity)
execCommandsWithCtx ctx conf opts = do
  let configParser =
        mkConfigParser ctx $ compose Identity (conf :* hiddenSources)
      (_, dummyCommands) =
        mapSubcommand () (allToDummyOpts @String opts)
      dummyParser =
        Optparse.subparser (mconcat dummyCommands)

  config <- getConfig ctx configParser dummyParser
  sourceVals <- getSource ctx config

  let (errs, commands) =
        mapSubcommand sourceVals (mapAssocList (compose Identity) opts)
      optParser =
        Optparse.subparser (mconcat commands)
      -- parser that includes the configuration options, otherwise parsing
      -- will find more options and fail
      allParser =
        (,) <$> optParser <*> configParser
  fst
    <$> if null errs
      then execParser ctx allParser
      else failParser allParser errs

-- | Run the subcommand parser and combine with values from the specified
-- sources
execCommands ::
  forall c ts xs.
  ( B.TraversableB (VariantF xs),
    B.TraversableB c,
    B.ApplicativeB c,
    Subcommands ts xs,
    GetSource c Identity,
    All (RunSource (SourceVal (c :* HiddenSources))) xs,
    All (RunSource ()) xs,
    MapAssocList xs
  ) =>
  -- | Source options
  c Opt ->
  -- | Target options associated with subcommands
  AssocListF ts xs Opt ->
  IO (VariantF xs Identity)
execCommands conf opts = do
  ctx <- getCtx
  execCommandsWithCtx ctx conf opts

-- | Run the subcommand parser only with default sources (environment
-- variables), passing the context explicitly.
execCommandsWithCtxDef ::
  forall ts xs.
  ( B.TraversableB (VariantF xs),
    Subcommands ts xs,
    All (RunSource (SourceVal (DefaultSources :* HiddenSources))) xs,
    All (RunSource ()) xs,
    MapAssocList xs
  ) =>
  -- | Context containing the environment and the cmdline args
  HargCtx ->
  -- | Target options associated with subcommands
  AssocListF ts xs Opt ->
  IO (VariantF xs Identity)
execCommandsWithCtxDef ctx =
  execCommandsWithCtx ctx defaultSources

-- | Run the subcommand parser only with default sources (environment
-- variables)
execCommandsDef ::
  forall ts xs.
  ( B.TraversableB (VariantF xs),
    Subcommands ts xs,
    All (RunSource (SourceVal (DefaultSources :* HiddenSources))) xs,
    All (RunSource ()) xs,
    MapAssocList xs
  ) =>
  -- | Target options associated with subcommands
  AssocListF ts xs Opt ->
  IO (VariantF xs Identity)
execCommandsDef =
  execCommands defaultSources

-- | Run the optparse-applicative parser, printing accumulated errors. Errors
-- are printed as warnings if the parser succeeds.
execParser ::
  HargCtx ->
  Optparse.Parser a ->
  IO a
execParser HargCtx {..} parser =
  Optparse.handleParseResult (execParserPure _hcArgs parser)

failParser ::
  Optparse.Parser a ->
  [SourceRunError] ->
  IO a
failParser parser errs =
  Optparse.handleParseResult (Optparse.Failure failure)
  where
    failure =
      Optparse.parserFailure
        Optparse.defaultPrefs
        parserInfo
        (Optparse.ErrorMsg errStr)
        []
    parserInfo =
      Optparse.info (Optparse.helper <*> parser) Optparse.forwardOptions
    errStr =
      ppSourceRunErrors errs

-- | Run the optparse-applicative parser and return the
-- 'Optparse.ParserResult'
execParserPure ::
  [String] ->
  Optparse.Parser a ->
  Optparse.ParserResult a
execParserPure args parser =
  Optparse.execParserPure Optparse.defaultPrefs parserInfo args
  where
    parserInfo =
      Optparse.info (Optparse.helper <*> parser) Optparse.forwardOptions
