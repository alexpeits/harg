module Options.Harg.Help where

import           Data.Maybe                 (fromMaybe)

import           Options.Harg.Types

mkHelp
  :: Opt a
  -> String
mkHelp Opt{..}
  =  fromMaybe "" _optHelp
  <> maybe "" (\v -> " (env var: " <> v <> ")") _optEnvVar
