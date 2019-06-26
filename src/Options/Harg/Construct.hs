module Options.Harg.Construct where

import           Data.Kind                 (Type)

import           Options.Harg.Types

-- short
class HasLong (o :: Type -> Type) where
  optLong :: String -> o a -> o a

instance HasLong ArgOpt where
  optLong s o = o { _aLong = Just s }

instance HasLong FlagOpt where
  optLong s o = o { _sLong = Just s }

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
  optHelp s o = o { _aHelp = Just s }

instance HasHelp FlagOpt where
  optHelp s o = o { _sHelp = Just s }

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
  :: OptParser a
  -> ArgOpt a
arg p
  = ArgOpt
      { _aLong    = Nothing
      , _aShort   = Nothing
      , _aHelp    = Nothing
      , _aMetavar = Nothing
      , _aEnvVar  = Nothing
      , _aDefault = Nothing
      , _aParser  = p
      }

argWith
  :: OptParser a
  -> (ArgOpt a -> ArgOpt a)
  -> Opt a
argWith p f
  = mkOpt $ f (arg p)

flag
  :: a
  -> a
  -> FlagOpt a
flag d active
  = FlagOpt
      { _sLong    = Nothing
      , _sShort   = Nothing
      , _sHelp    = Nothing
      , _sEnvVar  = Nothing
      , _sDefault = d
      , _sActive  = active
      , _sParser  = const (pure d)
      }

flagWith
  :: a
  -> a
  -> (FlagOpt a -> FlagOpt a)
  -> Opt a
flagWith d active f
  = mkOpt $ f (flag d active)

switch :: FlagOpt Bool
switch
  = flag False True

switchWith
  :: (FlagOpt Bool -> FlagOpt Bool)
  -> Opt Bool
switchWith f
  = mkOpt $ f switch

switch' :: FlagOpt Bool
switch'
  = flag True False

switchWith'
  :: (FlagOpt Bool -> FlagOpt Bool)
  -> Opt Bool
switchWith' f
  = mkOpt $ f switch'
