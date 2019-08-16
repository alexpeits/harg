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


class HasLong o (attr :: [OptAttr]) where
  -- | Add a 'Options.Applicative.long' modifier to an option
  optLong :: String -> o attr a -> o attr a

instance HasLong OptionOpt a where
  optLong s o = o { _oLong = Just s }

instance HasLong FlagOpt a where
  optLong s o = o { _fLong = Just s }

class HasShort o (attr :: [OptAttr]) where
  -- | Add a 'Options.Applicative.short' modifier to an option
  optShort :: Char -> o attr a -> o attr a

instance HasShort OptionOpt a where
  optShort c o = o { _oShort = Just c }

instance HasShort FlagOpt a where
  optShort c o = o { _fShort = Just c }

class HasHelp o (attr :: [OptAttr]) where
  -- | Add 'Options.Applicative.help' to an option
  optHelp :: String -> o attr a -> o attr a

instance HasHelp OptionOpt a where
  optHelp s o = o { _oHelp = Just s }

instance HasHelp FlagOpt a where
  optHelp s o = o { _fHelp = Just s }

instance HasHelp ArgumentOpt a where
  optHelp s o = o { _aHelp = Just s }

class HasMetavar o (attr :: [OptAttr]) where
  -- | Add a 'Options.Applicative.metavar' metavar to an option, to be
  -- displayed as the meta-parameter next to long/short modifiers
  optMetavar :: String -> o attr a -> o attr a

instance HasMetavar OptionOpt a where
  optMetavar s o = o { _oMetavar = Just s }

instance HasMetavar ArgumentOpt a where
  optMetavar s o = o { _aMetavar = Just s }

class HasEnvVar o (attr :: [OptAttr]) where
  -- | Specify an environment variable to lookup for an option
  optEnvVar :: String -> o attr a -> o attr a

instance HasEnvVar OptionOpt a where
  optEnvVar s o = o { _oEnvVar = Just s }

instance HasEnvVar FlagOpt a where
  optEnvVar s o = o { _fEnvVar = Just s }

instance HasEnvVar ArgumentOpt a where
  optEnvVar s o = o { _aEnvVar = Just s }

class HasDefault o (attr :: [OptAttr]) where
  -- | Add a default value to an option. Cannot be used in conjuction with
  -- 'optOptional' or 'optDefaultStr'.
  optDefault
    :: ( NotInAttrs OptOptional attr "optDefault" "optOptional"
       , NotInAttrs OptDefaultStr attr "optDefault" "optDefaultStr"
       )
    => a -> o attr a -> o (OptDefault ': attr) a

instance HasDefault OptionOpt a where
  optDefault a o = o { _oDefault = Just a }

instance HasDefault ArgumentOpt a where
  optDefault a o = o { _aDefault = Just a }

class HasDefaultStr o (attr :: [OptAttr]) where
  -- | Add a default unparsed value to an option. Cannot be used in conjuction
  -- with 'optDefault' or 'optOptional'.
  optDefaultStr
    :: ( NotInAttrs OptOptional attr "optDefaultStr" "optOptional"
       , NotInAttrs OptDefault attr "optDefaultStr" "optDefault"
       )
    => String -> o attr a -> o (OptDefaultStr ': attr) a

instance HasDefaultStr OptionOpt a where
  optDefaultStr s o = o { _oDefaultStr = Just s }

instance HasDefaultStr ArgumentOpt a where
  optDefaultStr s o = o { _aDefaultStr = Just s }

-- | Class for options that can be optional. Cannot be used in conjunction with
-- 'HasDefault' or 'HasDefaultStr'. Note that this will turn a parser for @a@
-- into a parser for @Maybe a@, modifying the reader function appropriately.
-- For example:
--
-- @
--   someOpt :: Opt (Maybe Int)
--   someOpt
--     = optionWith readParser
--         ( optLong "someopt"
--         . optOptional
--         )
-- @
class HasOptional o (attr :: [OptAttr]) where
  -- | Specify that an option is optional. This will convert an @Opt a@ to an
  -- @Opt (Maybe a)@
  optOptional
    :: ( NotInAttrs OptDefault attr "optOptional" "optDefault"
       , NotInAttrs OptDefaultStr attr "optOptional" "optDefaultStr"
       )
    => o attr a -> o (OptOptional ': attr) (Maybe a)

instance HasOptional OptionOpt a where
  optOptional OptionOpt{..}
    = OptionOpt
        { _oLong       = _oLong
        , _oShort      = _oShort
        , _oHelp       = _oHelp
        , _oMetavar    = _oMetavar
        , _oEnvVar     = _oEnvVar
        , _oDefault    = Just Nothing
        , _oDefaultStr = Nothing
        , _oReader     = fmap Just . _oReader
        }

instance HasOptional ArgumentOpt a where
  optOptional ArgumentOpt{..}
    = ArgumentOpt
        { _aHelp       = _aHelp
        , _aMetavar    = _aMetavar
        , _aEnvVar     = _aEnvVar
        , _aDefault    = Just Nothing
        , _aDefaultStr = Nothing
        , _aReader     = fmap Just . _aReader
        }

-- | Class to convert an intermediate option type into 'Opt'. Instances
-- should set the appropriate '_optType'.
class IsOpt o (attr :: [OptAttr]) where
  -- | Convert an intermediate option to an 'Opt'
  toOpt :: o attr a -> Opt a

instance IsOpt OptionOpt attr where
  toOpt OptionOpt{..}
    = Opt
        { _optLong       = _oLong
        , _optShort      = _oShort
        , _optHelp       = _oHelp
        , _optMetavar    = _oMetavar
        , _optEnvVar     = _oEnvVar
        , _optDefault    = _oDefault
        , _optDefaultStr = _oDefaultStr
        , _optReader     = _oReader
        , _optType       = OptionOptType
        }

instance IsOpt FlagOpt attr where
  toOpt FlagOpt{..}
    = Opt
        { _optLong       = _fLong
        , _optShort      = _fShort
        , _optHelp       = _fHelp
        , _optMetavar    = Nothing
        , _optEnvVar     = _fEnvVar
        , _optDefault    = Just _fDefault
        , _optDefaultStr = Nothing
        , _optReader     = _fReader
        , _optType       = FlagOptType _fActive
        }

instance IsOpt ArgumentOpt attr where
  toOpt ArgumentOpt{..}
    = Opt
        { _optLong       = Nothing
        , _optShort      = Nothing
        , _optHelp       = _aHelp
        , _optMetavar    = _aMetavar
        , _optEnvVar     = _aEnvVar
        , _optDefault    = _aDefault
        , _optDefaultStr = _aDefaultStr
        , _optReader     = _aReader
        , _optType       = ArgumentOptType
        }

-- | Create an option parser, equivalent to 'Options.Applicative.option'. The
-- result can then be used with 'toOpt' to convert into the global 'Opt' type.
--
-- @
--   someOption :: Opt Int
--   someOption
--     = toOpt ( option readParser
--             & optLong "someopt"
--             & optHelp "Some option"
--             & optDefault 256
--             )
-- @
option
  :: OptReader a
  -> OptionOpt '[] a
option p
  = OptionOpt
      { _oLong       = Nothing
      , _oShort      = Nothing
      , _oHelp       = Nothing
      , _oMetavar    = Nothing
      , _oEnvVar     = Nothing
      , _oDefault    = Nothing
      , _oDefaultStr = Nothing
      , _oReader     = p
      }

-- | Similar to 'option', but accepts a modifier function and returns an 'Opt'
-- directly.
--
-- @
--   someOption :: Opt Int
--   someOption
--     = optionWith readParser
--         ( optLong "someopt"
--         . optHelp "Some option"
--         . optDefault 256
--         )
-- @
optionWith
  :: OptReader a
  -> (OptionOpt '[] a -> OptionOpt attr b)
  -> Opt b
optionWith p f
  = toOpt $ f (option p)

-- | Create a flag parser, equivalent to 'Options.Applicative.option'. The
-- first argument is the default value (returned when the flag modifier is
-- absent), and the second is the active value (returned when the flag
-- modifier is present). The result can then be used with 'toOpt' to convert
-- into the global 'Opt' type.
--
-- @
--   someFlag :: Opt Int
--   someFlag
--     = toOpt ( flag 0 1
--             & optLong "someflag"
--             & optHelp "Some flag"
--             )
-- @
flag
  :: a  -- ^ Default value
  -> a  -- ^ Active value
  -> FlagOpt '[] a
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

-- | Similar to 'flag', but accepts a modifier function and returns an 'Opt'
-- directly.
--
-- @
--   someFlag :: Opt Int
--   someFlag
--     = flagWith 0 1
--         ( optLong "someflag"
--         . optHelp "Some flag"
--         )
-- @
flagWith
  :: a  -- ^ Default value
  -> a  -- ^ Active value
  -> (FlagOpt '[] a -> FlagOpt attr b)
  -> Opt b
flagWith d active f
  = toOpt $ f (flag d active)

-- | A 'flag' parser, specialized to 'Bool'. The parser (e.g. when parsing
-- an environment variable) will accept @true@ and @false@, but case
-- insensitive, rather than using the 'Read' instance for 'Bool'. The
-- default value is 'False', and the active value is 'True'.
--
-- @
--   someSwitch :: Opt Bool
--   someSwitch
--     = toOpt ( switch
--             & optLong "someswitch"
--             & optHelp "Some switch"
--             )
-- @
switch :: FlagOpt '[] Bool
switch
  = fl { _fReader = boolParser }
  where
    fl = flag False True

-- | Similar to 'switch', but accepts a modifier function and returns an 'Opt'
-- directly.
--
-- @
--   someSwitch :: Opt Bool
--   someSwitch
--     = switchWith
--         ( optLong "someswitch"
--         . optHelp "Some switch"
--         )
-- @
switchWith
  :: (FlagOpt '[] Bool -> FlagOpt attr Bool)
  -> Opt Bool
switchWith f
  = toOpt $ f switch

-- | Similar to 'switch', but the default value is 'True' and the active is
-- 'False'.
switch' :: FlagOpt '[] Bool
switch'
  = fl { _fReader = boolParser }
  where
    fl = flag True False

-- | Similar to 'switch'', but accepts a modifier function and returns an 'Opt'
-- directly.
switchWith'
  :: (FlagOpt '[] Bool -> FlagOpt attr Bool)
  -> Opt Bool
switchWith' f
  = toOpt $ f switch'

-- | Create an argument parser, equivalent to 'Options.Applicative.argument'.
-- The result can then be used with 'toOpt' to convert into the global 'Opt'
-- type.
--
-- @
--   someArgument :: Opt String
--   someArgument
--     = toOpt ( argument strParser
--             & optHelp "Some argument"
--             & optDefault "this is the default"
--             )
-- @
argument
  :: OptReader a
  -> ArgumentOpt '[] a
argument p
  = ArgumentOpt
      { _aHelp       = Nothing
      , _aMetavar    = Nothing
      , _aEnvVar     = Nothing
      , _aDefault    = Nothing
      , _aDefaultStr = Nothing
      , _aReader     = p
      }

-- | Similar to 'argument', but accepts a modifier function and returns an
-- 'Opt' directly.
--
-- @
--   someArgument :: Opt Int
--   someArgument
--     = argumentWith
--         ( optHelp "Some argument"
--         . optDefault "this is the default"
--         )
-- @
argumentWith
  :: OptReader a
  -> (ArgumentOpt '[] a -> ArgumentOpt attr b)
  -> Opt b
argumentWith p f
  = toOpt $ f (argument p)

-- | Convert a parser that returns 'Maybe' to a parser that returns 'Either',
-- with the default 'Left' value @unable to parse: \<input\>@.
parseWith
  :: (String -> Maybe a)  -- ^ Original parser
  -> String  -- ^ Input
  -> Either String a
parseWith parser s
  = maybe (Left err) Right (parser s)
  where
    err
      = "Unable to parse: " <> s

-- | A parser that uses the 'Read' instance to parse into a type.
readParser :: Read a => OptReader a
readParser
  = parseWith readMaybe

-- | A parser that returns a string. Any type that has an instance of
-- 'IsString' will work, and this parser always succeeds.
strParser
  :: IsString s
  => String
  -> Either String s
strParser
  = pure . fromString

-- | A parser that returns a 'Bool'. This will succeed for the strings
-- @true@ and @false@ in a case-insensitive manner.
boolParser :: String -> Either String Bool
boolParser s
  = case map toLower s of
      "true"  -> Right True
      "false" -> Right False
      _       -> Left ("Unable to parse " <> s <> " to Bool")


-- | Wrap a symbol in quotes, for pretty printing in type errors.
type QuoteSym (s :: Symbol)
  = 'Text "`" :<>: 'Text s :<>: 'Text "`"

-- | Check if `x` is not an element of the type-level list `xs`. If it is
-- print the appropriate error message using `l` and `r` for clarity.
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
