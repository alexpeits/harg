module Data.Origin.Options.Construct where

import           Data.Kind                 (Type)

import           Data.Origin.Options.Types


-- short
class Short (o :: Type -> Type) where
  optShort :: Char -> o a -> o a

instance Short ArgOpt where
  optShort c o = o { _aShort = Just c }

instance Short FlagOpt where
  optShort c o = o { _sShort = Just c }

-- help
class Help (o :: Type -> Type) where
  optHelp :: String -> o a -> o a

instance Help ArgOpt where
  optHelp s o = o { _aHelp = s }

instance Help FlagOpt where
  optHelp s o = o { _sHelp = s }

-- metavar
class Metavar (o :: Type -> Type) where
  optMetavar :: String -> o a -> o a

instance Metavar ArgOpt where
  optMetavar s o = o { _aMetavar = Just s }

instance Metavar FlagOpt where
  optMetavar s o = o { _sMetavar = Just s }

-- env var
class EnvVar (o :: Type -> Type) where
  optEnvVar :: String -> o a -> o a

instance EnvVar ArgOpt where
  optEnvVar s o = o { _aEnvVar = Just s }

instance EnvVar FlagOpt where
  optEnvVar s o = o { _sEnvVar = Just s }

-- default
class Default (o :: Type -> Type) where
  optDefault :: a -> o a -> o a

instance Default ArgOpt where
  optDefault a o = o { _aDefault = Just a }

-- convert from intermediate type to Opt
class ToOpt (o :: Type -> Type) where
  mkOpt :: o a -> Opt a

instance ToOpt ArgOpt where
  mkOpt ArgOpt{..}
    = Opt
        { _optLong    = _aLong
        , _optShort   = _aShort
        , _optHelp    = _aHelp
        , _optMetavar = _aMetavar
        , _optEnvVar  = _aEnvVar
        , _optDefault = _aDefault
        , _optParser  = _aParser
        , _optType    = ArgOptType
        }

instance ToOpt FlagOpt where
  mkOpt FlagOpt{..}
    = Opt
        { _optLong    = _sLong
        , _optShort   = _sShort
        , _optHelp    = _sHelp
        , _optMetavar = _sMetavar
        , _optEnvVar  = _sEnvVar
        , _optDefault = Just _sDefault
        , _optParser  = _sParser
        , _optType    = FlagOptType _sActive
        }

-- option constructors
option
  :: String
  -> OptParser a
  -> ArgOpt a
option long p
  = ArgOpt
      { _aLong    = long
      , _aShort   = Nothing
      , _aHelp    = ""
      , _aMetavar = Nothing
      , _aEnvVar  = Nothing
      , _aDefault = Nothing
      , _aParser  = p
      }

optionWith
  :: String
  -> OptParser a
  -> (ArgOpt a -> ArgOpt a)
  -> Opt a
optionWith long p f
  = mkOpt $ f (option long p)

flag
  :: String
  -> a
  -> a
  -> FlagOpt a
flag long d active
  = FlagOpt
      { _sLong    = long
      , _sShort   = Nothing
      , _sHelp    = ""
      , _sMetavar = Nothing
      , _sEnvVar  = Nothing
      , _sDefault = d
      , _sActive  = active
      , _sParser  = const (pure d)
      }

flagWith
  :: String
  -> a
  -> a
  -> (FlagOpt a -> FlagOpt a)
  -> Opt a
flagWith long d active f
  = mkOpt $ f (flag long d active)

switch :: String -> FlagOpt Bool
switch long
  = flag long False True

switchWith
  :: String
  -> (FlagOpt Bool -> FlagOpt Bool)
  -> Opt Bool
switchWith long f
  = mkOpt $ f (switch long)

switch' :: String -> FlagOpt Bool
switch' long
  = flag long True False

switchWith'
  :: String
  -> (FlagOpt Bool -> FlagOpt Bool)
  -> Opt Bool
switchWith' long f
  = mkOpt $ f (switch' long)
