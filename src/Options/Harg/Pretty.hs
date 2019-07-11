module Options.Harg.Pretty where

import Data.List          (intercalate, nubBy)
import Data.Maybe         (fromMaybe)

import Options.Harg.Types


ppHelp
  :: Opt a
  -> Maybe String
ppHelp Opt{..}
  = (<> ppEnvVar _optEnvVar) <$> _optHelp

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
  . nubBy cmpOptErr
  where
    cmpOptErr (OptError (SomeOpt l) sl dl) (OptError (SomeOpt r) sr dr)
      =  _optLong l == _optLong r && sl == sr && dl == dr
    ppOptError :: OptError -> String
    ppOptError (OptError (SomeOpt opt) src desc)
      =  "\t"
      <> fromMaybe "<no opt name>" (_optLong opt)
      <> "\t\t"
      <> desc
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
