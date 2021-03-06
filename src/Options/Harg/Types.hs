{-# LANGUAGE DeriveFunctor #-}

module Options.Harg.Types
  ( Opt (..),
    OptionOpt (..),
    FlagOpt (..),
    ArgumentOpt (..),
    OptAttr (..),
    OptType (..),
    SomeOpt (..),
    OptReader,
    HargCtx (..),
    Environment,
    getCtx,
    ctxFromArgs,
    ctxFromEnv,
    pureCtx,
  )
where

import System.Environment (getArgs, getEnvironment)

type OptReader a = String -> Either String a

-- | The basic option type
data Opt a = Opt
  { -- | Modifier for long options (e.g. @--user@)
    _optLong :: Maybe String,
    -- | Modifier for short options (e.g. @-u@)
    _optShort :: Maybe Char,
    -- | Option help to be shown when invoked
    --   with @--help/-h@ or in case of error
    _optHelp :: Maybe String,
    -- | Metavar to be shown in the help description
    _optMetavar :: Maybe String,
    -- | Environment variable for use with 'EnvSource'
    _optEnvVar :: Maybe String,
    -- | Default value
    _optDefaultVal :: Maybe a,
    -- | Default value as string (unparsed)
    _optDefaultStr :: Maybe String,
    -- | Option parser
    _optReader :: OptReader a,
    -- | Option type
    _optType :: OptType a
  }
  deriving (Functor)

-- | Option types
data OptType a
  = OptionOptType
  | -- | @a@ is the active value for the flag parser
    FlagOptType a
  | ArgumentOptType
  deriving (Functor)

data OptAttr
  = OptDefault
  | OptOptional

-- * Intermediate option types

-- | Option for flags with arguments. Corresponds to 'Options.Applicative.option'.
data OptionOpt (attr :: [OptAttr]) a = OptionOpt
  { _oLong :: Maybe String,
    _oShort :: Maybe Char,
    _oHelp :: Maybe String,
    _oMetavar :: Maybe String,
    _oEnvVar :: Maybe String,
    _oDefaultVal :: Maybe a,
    _oDefaultStr :: Maybe String,
    _oReader :: OptReader a
  }

-- | Option for flags that act like switches between a default and an active
-- value. Corresponds to 'Options.Applicative.flag'.
data FlagOpt (attr :: [OptAttr]) a = FlagOpt
  { _fLong :: Maybe String,
    _fShort :: Maybe Char,
    _fHelp :: Maybe String,
    _fEnvVar :: Maybe String,
    _fDefaultVal :: a,
    _fReader :: OptReader a,
    _fActive :: a
  }

-- | Option for arguments (no long/short specifiers). Corresponds to
-- 'Options.Applicative.argument'.
data ArgumentOpt (attr :: [OptAttr]) a = ArgumentOpt
  { _aHelp :: Maybe String,
    _aMetavar :: Maybe String,
    _aEnvVar :: Maybe String,
    _aDefaultVal :: Maybe a,
    _aDefaultStr :: Maybe String,
    _aReader :: OptReader a
  }

-- | Existential wrapper for 'Opt', so that many options can be carried in
-- a list.
data SomeOpt where
  SomeOpt :: Opt a -> SomeOpt

-- | Environment variable pairs, can be retrieved with 'getEnvironment'.
type Environment =
  [(String, String)]

-- | Command line arguments, can be retrieved with 'getArgs'.
type Args =
  [String]

-- | Context to carry around, that contains environment variables and
-- command line arguments.
data HargCtx = HargCtx
  { _hcEnv :: Environment,
    _hcArgs :: Args
  }

getCtx :: IO HargCtx
getCtx =
  HargCtx <$> getEnvironment <*> getArgs

ctxFromArgs :: Args -> IO HargCtx
ctxFromArgs args =
  HargCtx <$> getEnvironment <*> pure args

ctxFromEnv :: Environment -> IO HargCtx
ctxFromEnv env =
  HargCtx env <$> getArgs

pureCtx :: Environment -> Args -> HargCtx
pureCtx =
  HargCtx
