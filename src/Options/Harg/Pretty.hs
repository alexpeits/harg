module Options.Harg.Pretty where

import           Data.List                   (intercalate)

import           Options.Harg.Types

ppError :: OptError -> String
ppError
  = \case
      OptNotPresent someOpts
        -> "Missing options:\n" <> ppOpts someOpts
      OptInvalid details
        -> "Error while parsing options:\n" <> ppDetails details

ppOpts :: [SomeOpt] -> String
ppOpts
  = ppStrings . map ppOpt

ppOpt :: SomeOpt -> String
ppOpt (SomeOpt opt)
  = _optLong opt <> "\t\t" <> mkHelp opt

ppDetails :: [OptInvalidDetail] -> String
ppDetails
  = ppStrings . map ppDetail

ppDetail :: OptInvalidDetail -> String
ppDetail (OptInvalidDetail (SomeOpt opt) detail)
  = _optLong opt <> "\t\t" <> detail

ppStrings :: [String] -> String
ppStrings
  = intercalate "\n" . map ("\t" <>)

mkHelp
  :: Opt a
  -> String
mkHelp opt
  =  _optHelp opt
  <> maybe
       ""
       (\v -> " (env var: " <> v <> ")")
       (_optEnvVar opt)
