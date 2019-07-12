module Options.Harg.Cmdline where

import           Control.Applicative  ((<|>))
import           Data.Functor.Compose (Compose (..))
import           Data.List            (foldl')
import           Data.Maybe           (fromMaybe)

import qualified Data.Barbie          as B
import qualified Options.Applicative  as Optparse

import           Options.Harg.Pretty
import           Options.Harg.Types


mkOptparseParser
  :: forall f a.
     ( Applicative f
     , B.TraversableB a
     , B.ProductB a
     )
  => [a (Compose Maybe f)]
  -> a (Compose Opt f)
  -> Optparse.Parser (a f)
mkOptparseParser sources opts
  = let
      srcOpts
        = foldl'
            (B.bzipWith (<|>))
            (B.bmap (const (Compose Nothing)) opts)
            sources
    in B.bsequence $ B.bzipWith mkParser srcOpts opts

mkParser
  :: Compose Maybe f a
  -> Compose Opt f a
  -> Compose Optparse.Parser f a
mkParser srcs opt@(Compose Opt{..})
  = case _optType of
      OptionOptType      -> toOptionParser srcs opt
      FlagOptType active -> toFlagParser srcs opt active
      ArgumentOptType    -> toArgumentParser srcs opt

toOptionParser
  :: Compose Maybe f a
  -> Compose Opt f a
  -> Compose Optparse.Parser f a
toOptionParser sources (Compose opt@Opt{..})
  = Compose $ Optparse.option (Optparse.eitherReader _optReader)
      ( foldMap (fromMaybe mempty)
          [ Optparse.long <$> _optLong
          , Optparse.short <$> _optShort
          , Optparse.help <$> ppHelp opt
          , Optparse.metavar <$> _optMetavar
          , Optparse.value <$> (getCompose sources <|> _optDefault)
          ]
      )

toFlagParser
  :: Compose Maybe f a
  -> Compose Opt f a
  -> f a
  -> Compose Optparse.Parser f a
toFlagParser sources (Compose opt@Opt{..}) active
  =
    let
      mDef
        = case getCompose sources of
            Nothing -> _optDefault
            Just x  -> Just x
      modifiers
        = foldMap (fromMaybe mempty)
            [ Optparse.long <$> _optLong
            , Optparse.short <$> _optShort
            , Optparse.help <$> ppHelp opt
            ]
      in Compose $ case mDef of
           Nothing ->
             Optparse.flag' active modifiers
           Just def ->
             Optparse.flag def active modifiers

toArgumentParser
  :: Compose Maybe f a
  -> Compose Opt f a
  -> Compose Optparse.Parser f a
toArgumentParser sources (Compose opt@Opt{..})
  = Compose $ Optparse.argument (Optparse.eitherReader _optReader)
      ( foldMap (fromMaybe mempty)
          [ Optparse.help <$> ppHelp opt
          , Optparse.metavar <$> _optMetavar
          , Optparse.value <$> (getCompose sources <|> _optDefault)
          ]
      )
