{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Operations where

import           Data.Functor.Identity           (Identity(..))

import qualified Data.Barbie                     as B
import qualified Options.Applicative             as Optparse

import           Options.Harg.Cmdline            (mkOptparseParser)
import           Options.Harg.Config             (mkConfigParser, getConfig)
import           Options.Harg.Het.All            (All)
import           Options.Harg.Het.HList          (AssocListF, MapAssocList(..))
import           Options.Harg.Het.Prod           ((:*)(..))
import           Options.Harg.Het.Variant        (VariantF)
import           Options.Harg.Pretty             (ppWarning, ppError)
import           Options.Harg.Sources            ( accumSourceResults
                                                 , DefaultSources, defaultSources
                                                 , HiddenSources, hiddenSources
                                                 )
import           Options.Harg.Sources.Types      (GetSource(..), RunSource(..))
import           Options.Harg.Subcommands        (Subcommands(..))
import           Options.Harg.Types              (HargCtx(..), getCtx, Opt, OptError)
import           Options.Harg.Util               (toDummyOpts, allToDummyOpts, compose)


-- | Run the option parser and combine with values from the specified sources,
-- passing the context explicitly.
execOptWithCtx
  :: forall c a.
     ( B.TraversableB a
     , B.ProductB a
     , B.TraversableB c
     , B.ProductB c
     , GetSource c Identity
     , RunSource (SourceVal c) a
     )
  => HargCtx  -- ^ Context containing the environment and the cmdline args
  -> c Opt    -- ^ Source options
  -> a Opt    -- ^ Target configuration options
  -> IO (a Identity)
execOptWithCtx ctx conf opts
  = do
      let
        configParser = mkConfigParser ctx $ compose Identity (conf :* hiddenSources)
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

-- | Run the option parser and combine with values from the specified sources
execOpt
  :: forall c a.
     ( B.TraversableB a
     , B.ProductB a
     , B.TraversableB c
     , B.ProductB c
     , GetSource c Identity
     , RunSource (SourceVal c) a
     )
  => c Opt  -- ^ Source options
  -> a Opt  -- ^ Target configuration options
  -> IO (a Identity)
execOpt conf opts
  = do
      ctx <- getCtx
      execOptWithCtx ctx conf opts

-- | Run the option parser only with default sources (environment variables),
-- passing the context explicitly.
execOptWithCtxDef
  :: forall a.
     ( B.TraversableB a
     , B.ProductB a
     )
  => HargCtx  -- ^ Context containing the environment and the cmdline args
  -> a Opt    -- ^ Target configuration options
  -> IO (a Identity)
execOptWithCtxDef ctx
  = execOptWithCtx ctx defaultSources

-- | Run the option parser only with default sources (environment variables)
execOptDef
  :: forall a.
     ( B.TraversableB a
     , B.ProductB a
     )
  => a Opt -- ^ Target configuration options
  -> IO (a Identity)
execOptDef
  = execOpt defaultSources

-- | Run the subcommand parser and combine with values from the specified
-- sources, passing the context explicitly.
execCommandsWithCtx
  :: forall c ts xs.
     ( B.TraversableB (VariantF xs)
     , B.TraversableB c
     , B.ProductB c
     , Subcommands ts xs
     , GetSource c Identity
     , All (RunSource (SourceVal (c :* HiddenSources))) xs
     , All (RunSource ()) xs
     , MapAssocList xs
     )
  => HargCtx  -- ^ Context containing the environment and the cmdline args
  -> c Opt    -- ^ Source options
  -> AssocListF ts xs Opt  -- ^ Target options associated with subcommands
  -> IO (VariantF xs Identity)
execCommandsWithCtx ctx conf opts
  = do
      let
        configParser = mkConfigParser ctx $ compose Identity (conf :* hiddenSources)
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

-- | Run the subcommand parser and combine with values from the specified
-- sources
execCommands
  :: forall c ts xs.
     ( B.TraversableB (VariantF xs)
     , B.TraversableB c
     , B.ProductB c
     , Subcommands ts xs
     , GetSource c Identity
     , All (RunSource (SourceVal (c :* HiddenSources))) xs
     , All (RunSource ()) xs
     , MapAssocList xs
     )
  => c Opt  -- ^ Source options
  -> AssocListF ts xs Opt  -- ^ Target options associated with subcommands
  -> IO (VariantF xs Identity)
execCommands conf opts
  = do
      ctx <- getCtx
      execCommandsWithCtx ctx conf opts

-- | Run the subcommand parser only with default sources (environment
-- variables), passing the context explicitly.
execCommandsWithCtxDef
  :: forall ts xs.
     ( B.TraversableB (VariantF xs)
     , Subcommands ts xs
     , All (RunSource (SourceVal (DefaultSources :* HiddenSources))) xs
     , All (RunSource ()) xs
     , MapAssocList xs
     )
  => HargCtx  -- ^ Context containing the environment and the cmdline args
  -> AssocListF ts xs Opt  -- ^ Target options associated with subcommands
  -> IO (VariantF xs Identity)
execCommandsWithCtxDef ctx
  = execCommandsWithCtx ctx defaultSources

-- | Run the subcommand parser only with default sources (environment
-- variables)
execCommandsDef
  :: forall ts xs.
     ( B.TraversableB (VariantF xs)
     , Subcommands ts xs
     , All (RunSource (SourceVal (DefaultSources :* HiddenSources))) xs
     , All (RunSource ()) xs
     , MapAssocList xs
     )
  => AssocListF ts xs Opt  -- ^ Target options associated with subcommands
  -> IO (VariantF xs Identity)
execCommandsDef
  = execCommands defaultSources

-- | Run the optparse-applicative parser, printing accumulated errors. Errors
-- are printed as warnings if the parser succeeds.
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

-- | Run the optparse-applicative parser and return the
-- 'Optparse.ParserResult'
execParserPure
  :: [String]
  -> Optparse.Parser a
  -> Optparse.ParserResult a
execParserPure args parser
  = let
      parserInfo
        = Optparse.info (Optparse.helper <*> parser) Optparse.forwardOptions
    in Optparse.execParserPure Optparse.defaultPrefs parserInfo args
