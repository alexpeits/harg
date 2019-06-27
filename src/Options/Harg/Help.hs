module Options.Harg.Help where

import           Options.Harg.Types

mkHelp
  :: Opt a
  -> Maybe String
mkHelp Opt{..}
  = (<> envVarHelp) <$> _optHelp
  where
    envVarHelp
      = maybe "" (\v -> " (env var: " <> v <> ")") _optEnvVar
