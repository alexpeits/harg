module Options.Harg.Pretty where

import           Data.List          (intercalate)
import           Data.Maybe         (fromMaybe)

import           Options.Harg.Types

ppWarning
  :: [OptError]
  -> IO ()
ppWarning []
  = pure ()
ppWarning err
  =  putStrLn "Parser succeeded with warnings when parsing env vars:"
  >> ppOptErrors err
  >> putStrLn ""

ppError
  :: [OptError]
  -> IO ()
ppError []
  = pure ()
ppError err
  =  putStrLn "Parser errors when parsing env vars:"
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
      <> " (env var:"
      <> fromMaybe "<no env vars>" (_optEnvVar opt)
      <> ")"
