module Options.Harg.Cmdline where

import           Control.Applicative        ((<|>))
import           Data.Maybe                 (fromMaybe)

import qualified Options.Applicative        as Optparse

import           Options.Harg.Env
import           Options.Harg.Help
import           Options.Harg.Types


toParser :: Opt a -> IO (Parser a)
toParser opt@Opt{..} = do
  envVarRes <- tryParseEnvVar opt

  let
    envVar
      = getEnvVar envVarRes
    err
      = case envVarRes of
          EnvVarFoundNoParse detail -> [OptError (SomeOpt opt) detail]
          _ -> []

  case _optType of

    OptionOptType      -> toOptionParser opt envVar err

    FlagOptType active -> toFlagParser opt active envVar err

    ArgumentOptType    -> toArgumentParser opt envVar err

toOptionParser
  :: Opt a
  -> Maybe a  -- env var
  -> [OptError]
  -> IO (Parser a)
toOptionParser opt@Opt{..} envVar err
  = do
      let
        optionOpt
          = Optparse.option (Optparse.eitherReader _optReader)
              ( foldMap (fromMaybe mempty)
                  [ Optparse.long <$> _optLong
                  , Optparse.short <$> _optShort
                  , Optparse.help <$> mkHelp opt
                  , Optparse.metavar <$> _optMetavar
                  , Optparse.value <$> (envVar <|> _optDefault)
                  ]
              )
      pure $ Parser optionOpt err

toFlagParser
  :: Opt a
  -> a -- active value
  -> Maybe a  -- env var
  -> [OptError]
  -> IO (Parser a)
toFlagParser opt@Opt{..} active envVar err
  = do
      let
        mDef
          = case envVar of
              Nothing -> _optDefault
              Just _  -> Just active
        modifiers
          = foldMap (fromMaybe mempty)
              [ Optparse.long <$> _optLong
              , Optparse.short <$> _optShort
              , Optparse.help <$> mkHelp opt
              ]
        flagOpt
          = case mDef of
              Nothing ->
                Optparse.flag' active modifiers
              Just def ->
                Optparse.flag def active modifiers

      pure $ Parser flagOpt err

toArgumentParser
  :: Opt a
  -> Maybe a  -- env var
  -> [OptError]
  -> IO (Parser a)
toArgumentParser opt@Opt{..} envVar err
  = do
      let
        argOpt
          = Optparse.argument (Optparse.eitherReader _optReader)
              ( foldMap (fromMaybe mempty)
                  [ Optparse.help <$> mkHelp opt
                  , Optparse.metavar <$> _optMetavar
                  , Optparse.value <$> (envVar <|> _optDefault)
                  ]
              )

      pure $ Parser argOpt err
