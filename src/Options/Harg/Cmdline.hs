module Options.Harg.Cmdline where

import           Data.Maybe           (fromMaybe)
import           Data.Foldable        (asum)

import qualified Options.Applicative  as Optparse

import           Options.Harg.Help
import           Options.Harg.Sources
import           Options.Harg.Types


toParser :: [ParserSource] -> Opt a -> Parser a
toParser sources opt@Opt{..}
  = let
      (res, errs)
        = accumSourceResults (runSources sources opt)
      parser
        = case _optType of
            OptionOptType      -> toOptionParser opt res
            FlagOptType active -> toFlagParser opt active res
            ArgumentOptType    -> toArgumentParser opt res

    in Parser parser errs

toOptionParser
  :: Opt a
  -> [Maybe a]
  -> Optparse.Parser a
toOptionParser opt@Opt{..} sources
  = Optparse.option (Optparse.eitherReader _optReader)
      ( foldMap (fromMaybe mempty)
          [ Optparse.long <$> _optLong
          , Optparse.short <$> _optShort
          , Optparse.help <$> mkHelp opt
          , Optparse.metavar <$> _optMetavar
          , Optparse.value <$> asum (sources ++ [_optDefault])
          ]
      )

toFlagParser
  :: Opt a
  -> a -- active value
  -> [Maybe a]  -- env var
  -> Optparse.Parser a
toFlagParser opt@Opt{..} active sources
  = let
      mDef
        = case asum sources of
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
  :: Opt a
  -> [Maybe a]  -- env var
  -> Optparse.Parser a
toArgumentParser opt@Opt{..} sources
  = Optparse.argument (Optparse.eitherReader _optReader)
      ( foldMap (fromMaybe mempty)
          [ Optparse.help <$> mkHelp opt
          , Optparse.metavar <$> _optMetavar
          , Optparse.value <$> asum (sources ++ [_optDefault])
          ]
      )
