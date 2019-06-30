module Options.Harg.Construct where

import           Data.Kind          (Type)
import           Text.Read          (readMaybe)

import           Options.Harg.Types

-- long
class HasLong (o :: Type -> Type) where
  long :: String -> o a -> o a

instance HasLong OptionOpt where
  long s o = o { _oLong = Just s }

instance HasLong FlagOpt where
  long s o = o { _sLong = Just s }

-- short
class HasShort (o :: Type -> Type) where
  short :: Char -> o a -> o a

instance HasShort OptionOpt where
  short c o = o { _oShort = Just c }

instance HasShort FlagOpt where
  short c o = o { _sShort = Just c }

-- help
class HasHelp (o :: Type -> Type) where
  help :: String -> o a -> o a

instance HasHelp OptionOpt where
  help s o = o { _oHelp = Just s }

instance HasHelp FlagOpt where
  help s o = o { _sHelp = Just s }

instance HasHelp ArgumentOpt where
  help s o = o { _aHelp = Just s }

-- metavar
class HasMetavar (o :: Type -> Type) where
  metavar :: String -> o a -> o a

instance HasMetavar OptionOpt where
  metavar s o = o { _oMetavar = Just s }

instance HasMetavar ArgumentOpt where
  metavar s o = o { _aMetavar = Just s }

-- env var
class HasEnvVar (o :: Type -> Type) where
  envVar :: String -> o a -> o a

instance HasEnvVar OptionOpt where
  envVar s o = o { _oEnvVar = Just s }

instance HasEnvVar FlagOpt where
  envVar s o = o { _sEnvVar = Just s }

instance HasEnvVar ArgumentOpt where
  envVar s o = o { _aEnvVar = Just s }

-- default
class HasDefault (o :: Type -> Type) where
  def :: a -> o a -> o a

instance HasDefault OptionOpt where
  def a o = o { _oDefault = Just a }

instance HasDefault ArgumentOpt where
  def a o = o { _aDefault = Just a }

-- convert from intermediate type to Opt
class IsOpt (o :: Type -> Type) where
  toOpt :: o a -> Opt a

instance IsOpt OptionOpt where
  toOpt OptionOpt{..}
    = Opt
        { _optLong    = _oLong
        , _optShort   = _oShort
        , _optHelp    = _oHelp
        , _optMetavar = _oMetavar
        , _optEnvVar  = _oEnvVar
        , _optDefault = _oDefault
        , _optReader  = _oReader
        , _optType    = OptionOptType
        }

instance IsOpt FlagOpt where
  toOpt FlagOpt{..}
    = Opt
        { _optLong    = _sLong
        , _optShort   = _sShort
        , _optHelp    = _sHelp
        , _optMetavar = Nothing
        , _optEnvVar  = _sEnvVar
        , _optDefault = Just _sDefault
        , _optReader  = _sReader
        , _optType    = FlagOptType _sActive
        }

instance IsOpt ArgumentOpt where
  toOpt ArgumentOpt{..}
    = Opt
        { _optLong    = _aLong
        , _optShort   = _aShort
        , _optHelp    = _aHelp
        , _optMetavar = _aMetavar
        , _optEnvVar  = _aEnvVar
        , _optDefault = _aDefault
        , _optReader  = _aReader
        , _optType    = ArgumentOptType
        }

-- option constructors
option
  :: OptReader a
  -> OptionOpt a
option p
  = OptionOpt
      { _oLong    = Nothing
      , _oShort   = Nothing
      , _oHelp    = Nothing
      , _oMetavar = Nothing
      , _oEnvVar  = Nothing
      , _oDefault = Nothing
      , _oReader  = p
      }

optionWith
  :: OptReader a
  -> (OptionOpt a -> OptionOpt a)
  -> Opt a
optionWith p f
  = toOpt $ f (option p)

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
      , _sReader  = const (pure d)
      }

flagWith
  :: a
  -> a
  -> (FlagOpt a -> FlagOpt a)
  -> Opt a
flagWith d active f
  = toOpt $ f (flag d active)

switch :: FlagOpt Bool
switch
  = flag False True

switchWith
  :: (FlagOpt Bool -> FlagOpt Bool)
  -> Opt Bool
switchWith f
  = toOpt $ f switch

switch' :: FlagOpt Bool
switch'
  = flag True False

switchWith'
  :: (FlagOpt Bool -> FlagOpt Bool)
  -> Opt Bool
switchWith' f
  = toOpt $ f switch'

argument
  :: OptReader a
  -> ArgumentOpt a
argument p
  = ArgumentOpt
      { _aLong    = Nothing
      , _aShort   = Nothing
      , _aHelp    = Nothing
      , _aMetavar = Nothing
      , _aEnvVar  = Nothing
      , _aDefault = Nothing
      , _aReader  = p
      }

argumentWith
  :: OptReader a
  -> (ArgumentOpt a -> ArgumentOpt a)
  -> Opt a
argumentWith p f
  = toOpt $ f (argument p)

-- option parsers
parseWith
  :: (String -> Maybe a)
  -> String
  -> Either String a
parseWith parser s
  = maybe (Left err) Right (parser s)
  where
    err
      = "Unable to parse: " <> s

readParser :: Read a => OptReader a
readParser
  = parseWith readMaybe

strParser :: String -> Either String String
strParser
  = pure
