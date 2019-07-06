module Options.Harg.Cmdline where

import           Control.Applicative   ((<|>))
import           Data.Functor.Identity (Identity (..))
import           Data.List             (foldl1', nub)
import           Data.Maybe            (fromMaybe)

import qualified Data.Barbie           as B
import qualified Options.Applicative   as Optparse

import           Options.Harg.Help
import           Options.Harg.Sources
import           Options.Harg.Types
import           Options.Harg.Util


mkOptparseParser
  :: forall a.
     ( B.TraversableB a
     , B.ProductB a
     )
  => [ParserSource]
  -> a Opt
  -> IO (Optparse.Parser (a Identity), [OptError])
mkOptparseParser sources opts
  = let
      (errs, res)
        = accumSourceResults (runSources sources opts)
      srcOpts
        = foldl1' (bpairwise (<|>)) res
      parser
        = B.bsequence' $ bpairwise mkParser srcOpts opts
    in pure (parser, nub errs)
  where
    mkParser srcs opt@Opt{..}
      = case _optType of
          OptionOptType      -> toOptionParser srcs opt
          FlagOptType active -> toFlagParser srcs opt active
          ArgumentOptType    -> toArgumentParser srcs opt

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
