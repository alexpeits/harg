{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Options.Harg.Cmdline where

import           Control.Applicative  ((<|>))
import           Data.Foldable        (asum)
import           Data.Maybe           (fromMaybe)

import qualified Options.Applicative  as Optparse

import           Options.Harg.Help
import           Options.Harg.Sources
import           Options.Harg.Types

toParser :: Maybe a -> Opt a -> Optparse.Parser a
toParser sources opt@Opt{..}
  = let
      parser
        = case _optType of
            OptionOptType      -> toOptionParser sources opt
            FlagOptType active -> toFlagParser sources opt active
            ArgumentOptType    -> toArgumentParser sources opt

    in parser

toOptionParser
  :: Maybe a
  -> Opt a
  -> Optparse.Parser a
toOptionParser sources opt@Opt{..}
  = Optparse.option (Optparse.eitherReader _optReader)
      ( foldMap (fromMaybe mempty)
          [ Optparse.long <$> _optLong
          , Optparse.short <$> _optShort
          , Optparse.help <$> mkHelp opt
          , Optparse.metavar <$> _optMetavar
          , Optparse.value <$> (sources <|> _optDefault)
          ]
      )

toFlagParser
  :: Maybe a
  -> Opt a
  -> a
  -> Optparse.Parser a
toFlagParser sources opt@Opt{..} active
  =
    let
      mDef
        = case sources of
            Nothing -> _optDefault
            Just _  -> Just active
      modifiers
        = foldMap (fromMaybe mempty)
            [ Optparse.long <$> _optLong
            , Optparse.short <$> _optShort
            , Optparse.help <$> mkHelp opt
            ]
      in case mDef of
           Nothing ->
             Optparse.flag' active modifiers
           Just def ->
             Optparse.flag def active modifiers

toArgumentParser
  :: Maybe a
  -> Opt a
  -> Optparse.Parser a
toArgumentParser sources opt@Opt{..}
  = Optparse.argument (Optparse.eitherReader _optReader)
      ( foldMap (fromMaybe mempty)
          [ Optparse.help <$> mkHelp opt
          , Optparse.metavar <$> _optMetavar
          , Optparse.value <$> (sources <|> _optDefault)
          ]
      )
