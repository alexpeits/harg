module Options.Harg.Pretty where

import Data.List          (intercalate)
import Data.Maybe         (fromMaybe)

import Options.Harg.Types


ppHelp
  :: Opt a
  -> Maybe String
ppHelp Opt{..}
  = (<> prettyEnvVar _optEnvVar) <$> _optHelp

ppWarning
  :: [OptError]
  -> IO ()
ppWarning []
  = pure ()
ppWarning err
  =  putStrLn "Parser succeeded with warnings:"
  >> ppOptErrors err
  >> putStrLn ""

ppError
  :: [OptError]
  -> IO ()
ppError []
  = pure ()
ppError err
  =  putStrLn "Parser errors:"
  >> ppOptErrors err
  >> putStrLn ""

ppOptErrors
  :: [OptError]
  -> IO ()
ppOptErrors
  = putStrLn
  . intercalate "\n"
  . map ppOptError
  where
    ppOptError :: OptError -> String
    ppOptError (OptError (SomeOpt opt) desc)
      =  "\t"
      <> fromMaybe "<no opt name>" (_optLong opt)
      <> "\t\t"
      <> desc
      <> prettyEnvVar (_optEnvVar opt)

prettyEnvVar
  :: Maybe String
  -> String
prettyEnvVar
  = maybe "" $ \s -> " (env var: " <> s <> ")"
