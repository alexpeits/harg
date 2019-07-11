{-# LANGUAGE DeriveFunctor #-}
module Options.Harg.Types where

import System.Environment (getArgs, getEnvironment)

type OptReader a = String -> Either String a

-- Option
data Opt a
  = Opt
      { _optLong    :: Maybe String
      , _optShort   :: Maybe Char
      , _optHelp    :: Maybe String
      , _optMetavar :: Maybe String
      , _optEnvVar  :: Maybe String
      , _optDefault :: Maybe a
      , _optReader  :: OptReader a
      , _optType    :: OptType a
      }
  deriving Functor

data OptType a
  = OptionOptType
  | FlagOptType a  -- active value
  | ArgumentOptType
  deriving Functor

-- Option for flags with arguments
data OptionOpt a
  = OptionOpt
      { _oLong    :: Maybe String
      , _oShort   :: Maybe Char
      , _oHelp    :: Maybe String
      , _oMetavar :: Maybe String
      , _oEnvVar  :: Maybe String
      , _oDefault :: Maybe a
      , _oReader  :: OptReader a
      }

-- Option for flags that act like switches between a default and an active
-- value
data FlagOpt a
  = FlagOpt
      { _fLong    :: Maybe String
      , _fShort   :: Maybe Char
      , _fHelp    :: Maybe String
      , _fEnvVar  :: Maybe String
      , _fDefault :: a
      , _fReader  :: OptReader a
      , _fActive  :: a
      }

-- Option for arguments (no long/short specifiers)
data ArgumentOpt a
  = ArgumentOpt
      { _aHelp    :: Maybe String
      , _aMetavar :: Maybe String
      , _aEnvVar  :: Maybe String
      , _aDefault :: Maybe a
      , _aReader  :: OptReader a
      }

data OptError
  = OptError
      { _oeOpt    :: SomeOpt
      , _oeSource :: String
      , _oeDesc   :: String
      }

data SomeOpt where
  SomeOpt :: Opt a -> SomeOpt

type Environment
  = [(String, String)]

type Args
  = [String]

data HargCtx
  = HargCtx
      { _hcEnv  :: Environment
      , _hcArgs :: Args
      }

getCtx :: IO HargCtx
getCtx
  = HargCtx <$> getEnvironment <*> getArgs

toOptError
  :: Opt a
  -> String
  -> String
  -> OptError
toOptError
  = OptError . SomeOpt
