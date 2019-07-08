{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Operations where

import           Data.Functor.Identity    (Identity(..))
import           System.Environment       (getArgs)

import qualified Data.Barbie              as B
import qualified Options.Applicative      as Optparse

import           Options.Harg.Cmdline     (mkOptparseParser)
import           Options.Harg.Het.All
import           Options.Harg.Het.HList
import           Options.Harg.Het.Nat
import           Options.Harg.Het.Variant
import           Options.Harg.Subcommands
import           Options.Harg.Pretty
import           Options.Harg.Sources
import           Options.Harg.Types
import           Options.Harg.Util


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

execOptDef
  :: forall a.
     ( B.TraversableB a
     , B.ProductB a
     )
  => a Opt
  -> IO (a Identity)
execOptDef
  = execOpt defaultSources

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

execCommandsDef
  :: forall ts xs.
     ( B.TraversableB (VariantF xs)
     , Subcommands Z ts xs '[]
     , All (RunSource '[SourceValFor EnvSource]) xs
     , All (RunSource '[]) xs
     , MapAssocList xs
     )
  => AssocListF ts xs Opt
  -> IO (VariantF xs Identity)
execCommandsDef
  = execCommands defaultSources

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
