module Options.Harg.Construct where

import           Data.Kind                 (Type)

import           Options.Harg.Types

-- short
class HasShort (o :: Type -> Type) where
  optShort :: Char -> o a -> o a

instance HasShort ArgOpt where
  optShort c o = o { _aShort = Just c }

instance HasShort FlagOpt where
  optShort c o = o { _sShort = Just c }

-- help
class HasHelp (o :: Type -> Type) where
  optHelp :: String -> o a -> o a

instance HasHelp ArgOpt where
  optHelp s o = o { _aHelp = s }

instance HasHelp FlagOpt where
  optHelp s o = o { _sHelp = s }

-- metavar
class HasMetavar (o :: Type -> Type) where
  optMetavar :: String -> o a -> o a

instance HasMetavar ArgOpt where
  optMetavar s o = o { _aMetavar = Just s }

-- env var
class HasEnvVar (o :: Type -> Type) where
  optEnvVar :: String -> o a -> o a

instance HasEnvVar ArgOpt where
  optEnvVar s o = o { _aEnvVar = Just s }

instance HasEnvVar FlagOpt where
  optEnvVar s o = o { _sEnvVar = Just s }

-- default
class HasDefault (o :: Type -> Type) where
  optDefault :: a -> o a -> o a

instance HasDefault ArgOpt where
  optDefault a o = o { _aDefault = Just a }

-- convert from intermediate type to Opt
class IsOpt (o :: Type -> Type) where
  mkOpt :: o a -> Opt a

instance IsOpt ArgOpt where
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

instance IsOpt FlagOpt where
  mkOpt FlagOpt{..}
    = Opt
        { _optLong    = _sLong
        , _optShort   = _sShort
        , _optHelp    = _sHelp
        , _optMetavar = Nothing
        , _optEnvVar  = _sEnvVar
        , _optDefault = Just _sDefault
        , _optParser  = _sParser
        , _optType    = FlagOptType _sActive
        }

-- option constructors
arg
  :: String
  -> OptParser a
  -> ArgOpt a
arg long p
  = ArgOpt
      { _aLong    = long
      , _aShort   = Nothing
      , _aHelp    = ""
      , _aMetavar = Nothing
      , _aEnvVar  = Nothing
      , _aDefault = Nothing
      , _aParser  = p
      }

argWith
  :: String
  -> OptParser a
  -> (ArgOpt a -> ArgOpt a)
  -> Opt a
argWith long p f
  = mkOpt $ f (arg long p)

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
