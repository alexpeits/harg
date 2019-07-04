{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Options.Harg.Cmdline where

import           Data.Maybe           (fromMaybe)
import           Data.Foldable        (asum)

import qualified Options.Applicative  as Optparse
import qualified Options.Applicative  as Optparse

import           Options.Harg.Help
import           Options.Harg.Sources
import           Options.Harg.Types

toParser :: [ParserSource] -> Opt a -> Opt a
toParser sources opt@Opt{..}
  = let
      parser
        = case _optType of
            OptionOptType _      -> opt { _optType = OptionOptType (toOptionParser opt) }
            FlagOptType _ active -> opt { _optType = FlagOptType (toFlagParser opt) active }
            ArgumentOptType _    -> opt { _optType = ArgumentOptType (toArgumentParser opt) }

    in parser

toOptionParser
  :: Opt a
  -> Optparse.Mod Optparse.OptionFields a
toOptionParser opt@Opt{..}
  = foldMap (fromMaybe mempty)
      [ Optparse.long <$> _optLong
      , Optparse.short <$> _optShort
      , Optparse.help <$> mkHelp opt
      , Optparse.metavar <$> _optMetavar
      -- , Optparse.value <$> asum (sources ++ [_optDefault])
      ]

toFlagParser
  :: Opt a
  -> Optparse.Mod Optparse.FlagFields a
toFlagParser opt@Opt{..}
  = foldMap (fromMaybe mempty)
      [ Optparse.long <$> _optLong
      , Optparse.short <$> _optShort
      , Optparse.help <$> mkHelp opt
      ]

toArgumentParser
  :: Opt a
  -> Optparse.Mod Optparse.ArgumentFields a
toArgumentParser opt@Opt{..}
  = foldMap (fromMaybe mempty)
      [ Optparse.help <$> mkHelp opt
      , Optparse.metavar <$> _optMetavar
      -- , Optparse.value <$> asum (sources ++ [_optDefault])
      ]
