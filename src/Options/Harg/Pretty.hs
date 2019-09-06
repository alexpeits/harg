module Options.Harg.Pretty where

import Data.List          (intercalate, nubBy)
import Data.Maybe         (fromMaybe)

import Options.Harg.Types


ppHelp
  :: Opt a
  -> Maybe String
ppHelp Opt{..}
  = (<> ppEnvVar _optEnvVar) <$> _optHelp

ppOptErrors
  :: [OptError]
  -> String
ppOptErrors
  = intercalate "\n\n"
  . map ppOptError
  . nubBy cmpOptErr
  where
    cmpOptErr (OptError (SomeOpt l) sl dl) (OptError (SomeOpt r) sr dr)
      =  _optLong l == _optLong r && sl == sr && dl == dr
    ppOptError :: OptError -> String
    ppOptError (OptError (SomeOpt opt) src desc)
      =  "option "
      <> fromMaybe "<no opt name>" (_optLong opt)
      <> ": "
      <> desc
      <> "\n\t"
      <> ppSource src
      <> ppEnvVar (_optEnvVar opt)

ppSource
  :: Maybe String
  -> String
ppSource
  = maybe "" $ \s -> " (source: " <> s <> ")"

ppEnvVar
  :: Maybe String
  -> String
ppEnvVar
  = maybe "" $ \s -> " (env var: " <> s <> ")"
