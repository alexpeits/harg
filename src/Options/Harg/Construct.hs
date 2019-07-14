{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Options.Harg.Construct where

import Data.Char          (toLower)
import Data.Kind          (Constraint)
import Data.String        (IsString(..))
import GHC.TypeLits       (ErrorMessage(..), TypeError, Symbol)
import Text.Read          (readMaybe)

import Options.Harg.Types


type QuoteSym (s :: Symbol)
  = 'Text "`" :<>: 'Text s :<>: 'Text "`"

type family NotInAttrs
    (x :: k)
    (xs :: [k])
    (l :: Symbol)
    (r :: Symbol)
    :: Constraint where
  NotInAttrs _ '[]  _ _
    = ()
  NotInAttrs x (x ': _) l r
    = TypeError
    (    QuoteSym l :<>: 'Text " and " :<>: QuoteSym r
    :<>: 'Text " cannot be mixed in an option definition."
    )
  NotInAttrs x (y ': xs) l r
    = NotInAttrs x xs l r

-- long
class HasLong o (attr :: [OptAttr]) where
  optLong :: String -> o attr a -> o attr a

instance HasLong OptionOpt a where
  optLong s o = o { _oLong = Just s }

instance HasLong FlagOpt a where
  optLong s o = o { _fLong = Just s }

-- short
class HasShort o (attr :: [OptAttr]) where
  optShort :: Char -> o attr a -> o attr a

instance HasShort OptionOpt a where
  optShort c o = o { _oShort = Just c }

instance HasShort FlagOpt a where
  optShort c o = o { _fShort = Just c }

-- help
class HasHelp o (attr :: [OptAttr]) where
  optHelp :: String -> o attr a -> o attr a

instance HasHelp OptionOpt a where
  optHelp s o = o { _oHelp = Just s }

instance HasHelp FlagOpt a where
  optHelp s o = o { _fHelp = Just s }

instance HasHelp ArgumentOpt a where
  optHelp s o = o { _aHelp = Just s }

-- metavar
class HasMetavar o (attr :: [OptAttr]) where
  optMetavar :: String -> o attr a -> o attr a

instance HasMetavar OptionOpt a where
  optMetavar s o = o { _oMetavar = Just s }

instance HasMetavar ArgumentOpt a where
  optMetavar s o = o { _aMetavar = Just s }

-- env var
class HasEnvVar o (attr :: [OptAttr]) where
  optEnvVar :: String -> o attr a -> o attr a

instance HasEnvVar OptionOpt a where
  optEnvVar s o = o { _oEnvVar = Just s }

instance HasEnvVar FlagOpt a where
  optEnvVar s o = o { _fEnvVar = Just s }

instance HasEnvVar ArgumentOpt a where
  optEnvVar s o = o { _aEnvVar = Just s }

-- default
class HasDefault o (attr :: [OptAttr]) where
  optDefault
    :: NotInAttrs OptOptional attr "optDefault" "optOptional"
    => a -> o attr a -> o (OptDefault ': attr) a

instance HasDefault OptionOpt a where
  optDefault a o = o { _oDefault = Just a }

instance HasDefault ArgumentOpt a where
  optDefault a o = o { _aDefault = Just a }

-- optional
class HasOptional o (attr :: [OptAttr]) where
  optOptional
    :: NotInAttrs OptDefault attr "optOptional" "optDefault"
    => o attr a -> o (OptOptional ': attr) (Maybe a)

instance HasOptional OptionOpt a where
  optOptional OptionOpt{..}
    = OptionOpt
        { _oLong    = _oLong
        , _oShort   = _oShort
        , _oHelp    = _oHelp
        , _oMetavar = _oMetavar
        , _oEnvVar  = _oEnvVar
        , _oDefault = Nothing
        , _oReader  = fmap Just . _oReader
        }

instance HasOptional ArgumentOpt a where
  optOptional ArgumentOpt{..}
    = ArgumentOpt
        { _aHelp    = _aHelp
        , _aMetavar = _aMetavar
        , _aEnvVar  = _aEnvVar
        , _aDefault = Nothing
        , _aReader  = fmap Just . _aReader
        }

-- convert from intermediate type to Opt
class IsOpt o (attr :: [OptAttr]) where
  toOpt :: o attr a -> Opt a

instance IsOpt OptionOpt attr where
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

instance IsOpt FlagOpt attr where
  toOpt FlagOpt{..}
    = Opt
        { _optLong    = _fLong
        , _optShort   = _fShort
        , _optHelp    = _fHelp
        , _optMetavar = Nothing
        , _optEnvVar  = _fEnvVar
        , _optDefault = Just _fDefault
        , _optReader  = _fReader
        , _optType    = FlagOptType _fActive
        }

instance IsOpt ArgumentOpt attr where
  toOpt ArgumentOpt{..}
    = Opt
        { _optLong    = Nothing
        , _optShort   = Nothing
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
  -> OptionOpt '[] a
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
  -> (OptionOpt '[] a -> OptionOpt attr a)
  -> Opt a
optionWith p f
  = toOpt $ f (option p)

flag
  :: a
  -> a
  -> FlagOpt attr a
flag d active
  = FlagOpt
      { _fLong    = Nothing
      , _fShort   = Nothing
      , _fHelp    = Nothing
      , _fEnvVar  = Nothing
      , _fDefault = d
      , _fActive  = active
      , _fReader  = const (pure d)  -- TODO
      }

flagWith
  :: a
  -> a
  -> (FlagOpt attr a -> FlagOpt attr a)
  -> Opt a
flagWith d active f
  = toOpt $ f (flag d active)

switch :: FlagOpt attr Bool
switch
  = fl { _fReader = boolParser }
  where
    fl = flag False True

switchWith
  :: (FlagOpt attr Bool -> FlagOpt attr Bool)
  -> Opt Bool
switchWith f
  = toOpt $ f switch

switch' :: FlagOpt attr Bool
switch'
  = fl { _fReader = boolParser }
  where
    fl = flag True False

switchWith'
  :: (FlagOpt attr Bool -> FlagOpt attr Bool)
  -> Opt Bool
switchWith' f
  = toOpt $ f switch'

argument
  :: OptReader a
  -> ArgumentOpt attr a
argument p
  = ArgumentOpt
      { _aHelp    = Nothing
      , _aMetavar = Nothing
      , _aEnvVar  = Nothing
      , _aDefault = Nothing
      , _aReader  = p
      }

argumentWith
  :: OptReader a
  -> (ArgumentOpt attr a -> ArgumentOpt attr a)
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

strParser
  :: IsString s
  => String
  -> Either String s
strParser
  = pure . fromString

boolParser :: String -> Either String Bool
boolParser s
  = case map toLower s of
      "true"  -> Right True
      "false" -> Right False
      _       -> Left ("Unable to parse " <> s <> "to Bool")
