module Options.Harg.Pretty where

import Control.Applicative        ((<|>))
import Data.List                  (intercalate)
import Data.Maybe                 (fromMaybe)

import Options.Harg.Sources.Types
import Options.Harg.Types


ppHelp
  :: Opt a
  -> Maybe String
ppHelp Opt{..}
  = (<> ppEnvVar _optEnvVar) <$> _optHelp

ppSourceRunErrors
  :: [SourceRunError]
  -> String
ppSourceRunErrors
  = intercalate "\n\n"
  . map ppSourceRunError
  where
    ppSourceRunError :: SourceRunError -> String
    ppSourceRunError (SourceRunError Nothing src desc)
      =  "error: "
      <> desc
      <> "\n\t"
      <> ppSource src

    ppSourceRunError (SourceRunError (Just (SomeOpt opt)) src desc)
      =  "option "
      <> optId opt
      <> ": "
      <> desc
      <> "\n\t"
      <> ppSource src
      <> ppEnvVar (_optEnvVar opt)

    optId Opt{..}
      = fromMaybe "<no name available>"
        $ _optLong <|> (pure <$> _optShort) <|> _optMetavar

ppSource
  :: String
  -> String
ppSource s
  = " (source: " <> s <> ")"

ppEnvVar
  :: Maybe String
  -> String
ppEnvVar
  = maybe "" $ \s -> " (env var: " <> s <> ")"
