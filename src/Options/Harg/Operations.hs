{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Options.Harg.Operations where
  -- ( execOpt
  -- , execOptDef
  -- , execOptWithCtx
  -- , execOptWithCtxDef
  -- , execCommands
  -- , execCommandsDef
  -- , execCommandsWithCtx
  -- , execCommandsWithCtxDef
  -- ) where

import           Control.Applicative        ((<|>))
import           Data.Functor.Compose       (Compose(..))
import           Data.Functor.Const         (Const (..))
import           Data.Functor.Identity      (Identity(..))
import           Data.List                  (foldl')
import qualified Data.Functor.Product  as P

import qualified Data.Barbie                as B
import qualified Options.Applicative        as Optparse

import           Options.Harg.Cmdline       (mkOptparseParser, execParserPure)
import           Options.Harg.Config        (mkConfigParser, getConfig)
import           Options.Harg.Het.All       (All)
import           Options.Harg.Het.HList     (AssocListF, MapAssocList(..))
import           Options.Harg.Het.Prod      ((:*)(..))
import           Options.Harg.Het.Variant   (VariantF)
import           Options.Harg.Pretty        (ppSourceRunErrors)
import           Options.Harg.Sources       ( accumSourceResults
                                            , DefaultSources, defaultSources
                                            , HiddenSources, hiddenSources
                                            )
import           Options.Harg.Sources.Types ( GetSource(..) , RunSource(..)
                                            , SourceRunError
                                            )
import           Options.Harg.Sources.Env
import           Options.Harg.Sources.Optparse
-- import           Options.Harg.Subcommands   (Subcommands(..))
import           Options.Harg.Types         (HargCtx(..), getCtx, Opt)
import           Options.Harg.Util          (toDummyOpts, allToDummyOpts, compose)

import GHC.Generics (Generic)

newtype FlipCompose a g f
  = FlipCompose (a (Compose f g))
  deriving stock (Generic)

instance B.FunctorB a => B.FunctorB (FlipCompose a g) where
  bmap :: (forall x. f x -> f' x) -> FlipCompose a g f -> FlipCompose a g f'
  bmap nat (FlipCompose afg) = FlipCompose $ B.bmap (lft nat) afg
    where
      lft :: (forall x. f x -> f' x) -> Compose f g b -> Compose f' g b
      lft nt (Compose fgb)
        = Compose (nt fgb)

instance B.TraversableB a => B.TraversableB (FlipCompose a g) where
  btraverse
    :: Applicative e
    => (forall x. f x -> e (f' x))
    -> FlipCompose a g f
    -> e (FlipCompose a g f')
  btraverse nat (FlipCompose afg) = FlipCompose <$> B.btraverse (lft nat) afg
    where
      lft
        :: Applicative ap
        => (forall x. f x -> ap (f' x))
        -> Compose f g b -> ap (Compose f' g b)
      lft nt (Compose fgb)
        = Compose <$> nt fgb

instance B.ProductB a => B.ProductB (FlipCompose a g) where
  bprod
    :: FlipCompose a g f
    -> FlipCompose a g f'
    -> FlipCompose a g (f `P.Product` f')
  bprod (FlipCompose l) (FlipCompose r)
    = FlipCompose $ B.bzipWith go l r
    where
      go (Compose x) (Compose y) = Compose (P.Pair x y)

  buniq :: (forall x. f x) -> FlipCompose a g f
  buniq x = FlipCompose $ B.buniq (Compose x)

execOptWithConf
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
execOptWithConf ctx@HargCtx{..} conf opts = do
  let
    combOpts :: (c :* FlipCompose a (Const String)) Opt
    combOpts = conf :* FlipCompose (toDummyOpts opts)
    (_, res)
      = accumSourceResults
      $ runSource (OptparseSourceVal _hcArgs, EnvSourceVal _hcEnv) (compose Identity conf)
    src
      = foldl'
          (B.bzipWith (<|>))
          (B.bmap (const (Compose Nothing)) conf)
          res
  undefined

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
execOptWithCtx ctx conf opts = do
  let
    configParser
      = mkConfigParser ctx $ compose Identity (conf :* hiddenSources)
    dummyParser
      = mkOptparseParser [] (toDummyOpts @String opts)
  config <- getConfig ctx configParser dummyParser
  sourceVals <- getSource ctx config
  let
    (errs, sources)
      = accumSourceResults
        $ runSource sourceVals (compose Identity opts)
    parsed
      = foldl'
          (B.bzipWith (<|>))
          (B.bmap (const (Compose Nothing)) opts)
          sources
  putStrLn "**"
  putStrLn $ ppSourceRunErrors errs
  pure $ case B.bsequence parsed of
    Nothing -> error "TODO"
    Just x -> x
    -- optParser
      -- = mkOptparseParser sources (compose Identity opts)
    -- parser that includes the configuration options, otherwise parsing
    -- will find more options and fail
    -- allParser
      -- = (,) <$> optParser <*> configParser
  -- fst <$> if null errs
  --         then execParser ctx allParser
  --         else failParser allParser errs

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
execOpt conf opts = do
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

-- -- | Run the subcommand parser and combine with values from the specified
-- -- sources, passing the context explicitly.
-- execCommandsWithCtx
--   :: forall c ts xs.
--      ( B.TraversableB (VariantF xs)
--      , B.TraversableB c
--      , B.ProductB c
--      , Subcommands ts xs
--      , GetSource c Identity
--      , All (RunSource (SourceVal (c :* HiddenSources))) xs
--      , All (RunSource ()) xs
--      , MapAssocList xs
--      )
--   => HargCtx  -- ^ Context containing the environment and the cmdline args
--   -> c Opt    -- ^ Source options
--   -> AssocListF ts xs Opt  -- ^ Target options associated with subcommands
--   -> IO (VariantF xs Identity)
-- execCommandsWithCtx ctx conf opts = do
--   let
--     configParser
--       = mkConfigParser ctx $ compose Identity (conf :* hiddenSources)
--     (_, dummyCommands)
--       = mapSubcommand () (allToDummyOpts @String opts)
--     dummyParser
--       = Optparse.subparser (mconcat dummyCommands)
--
--   config <- getConfig ctx configParser dummyParser
--   sourceVals <- getSource ctx config
--
--   let
--     (errs, commands)
--       = mapSubcommand sourceVals (mapAssocList (compose Identity) opts)
--     optParser
--       = Optparse.subparser (mconcat commands)
--     -- parser that includes the configuration options, otherwise parsing
--     -- will find more options and fail
--     allParser
--       = (,) <$> optParser <*> configParser
--   fst <$> if null errs
--           then execParser ctx allParser
--           else failParser allParser errs
--
-- -- | Run the subcommand parser and combine with values from the specified
-- -- sources
-- execCommands
--   :: forall c ts xs.
--      ( B.TraversableB (VariantF xs)
--      , B.TraversableB c
--      , B.ProductB c
--      , Subcommands ts xs
--      , GetSource c Identity
--      , All (RunSource (SourceVal (c :* HiddenSources))) xs
--      , All (RunSource ()) xs
--      , MapAssocList xs
--      )
--   => c Opt  -- ^ Source options
--   -> AssocListF ts xs Opt  -- ^ Target options associated with subcommands
--   -> IO (VariantF xs Identity)
-- execCommands conf opts = do
--   ctx <- getCtx
--   execCommandsWithCtx ctx conf opts
--
-- -- | Run the subcommand parser only with default sources (environment
-- -- variables), passing the context explicitly.
-- execCommandsWithCtxDef
--   :: forall ts xs.
--      ( B.TraversableB (VariantF xs)
--      , Subcommands ts xs
--      , All (RunSource (SourceVal (DefaultSources :* HiddenSources))) xs
--      , All (RunSource ()) xs
--      , MapAssocList xs
--      )
--   => HargCtx  -- ^ Context containing the environment and the cmdline args
--   -> AssocListF ts xs Opt  -- ^ Target options associated with subcommands
--   -> IO (VariantF xs Identity)
-- execCommandsWithCtxDef ctx
--   = execCommandsWithCtx ctx defaultSources
--
-- -- | Run the subcommand parser only with default sources (environment
-- -- variables)
-- execCommandsDef
--   :: forall ts xs.
--      ( B.TraversableB (VariantF xs)
--      , Subcommands ts xs
--      , All (RunSource (SourceVal (DefaultSources :* HiddenSources))) xs
--      , All (RunSource ()) xs
--      , MapAssocList xs
--      )
--   => AssocListF ts xs Opt  -- ^ Target options associated with subcommands
--   -> IO (VariantF xs Identity)
-- execCommandsDef
--   = execCommands defaultSources

-- | Run the optparse-applicative parser, printing accumulated errors. Errors
-- are printed as warnings if the parser succeeds.
execParser
  :: HargCtx
  -> Optparse.Parser a
  -> IO a
execParser HargCtx{..} parser
  = Optparse.handleParseResult (execParserPure _hcArgs parser)

failParser
  :: Optparse.Parser a
  -> [SourceRunError]
  -> IO a
failParser parser errs
  = Optparse.handleParseResult (Optparse.Failure failure)
  where
    failure
      = Optparse.parserFailure
          Optparse.defaultPrefs
          parserInfo
          (Optparse.ErrorMsg errStr)
          []
    parserInfo
      = Optparse.info (Optparse.helper <*> parser) Optparse.forwardOptions
    errStr
      = ppSourceRunErrors errs
