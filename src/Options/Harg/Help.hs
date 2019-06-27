module Options.Harg.Help where

import           Data.Maybe                 (fromMaybe)

import           Options.Harg.Types

mkHelp
  :: Opt a
  -> String
mkHelp opt
  =  fromMaybe "" (_optHelp opt)
  <> maybe
       ""
       (\v -> " (env var: " <> v <> ")")
       (_optEnvVar opt)
