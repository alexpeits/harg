{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Options.Harg.Construct where

import Data.Char          (toLower)
import Data.Kind          (Constraint)
import Data.String        (IsString(..))
import GHC.TypeLits       (ErrorMessage(..), TypeError, Symbol, AppendSymbol)
import Text.Read          (readMaybe)

import Data.List.Split    (splitOn)

import Options.Harg.Types


class HasLong o (attr :: [OptAttr]) where
  -- | Add a 'Options.Applicative.long' modifier to an option
  long :: String -> o attr a -> o attr a

instance HasLong OptionOpt a where
  long s o = o { _oLong = Just s }

instance HasLong FlagOpt a where
  long s o = o { _fLong = Just s }

class HasShort o (attr :: [OptAttr]) where
  -- | Add a 'Options.Applicative.short' modifier to an option
  short :: Char -> o attr a -> o attr a

instance HasShort OptionOpt a where
  short c o = o { _oShort = Just c }

instance HasShort FlagOpt a where
  short c o = o { _fShort = Just c }

class HasHelp o (attr :: [OptAttr]) where
  -- | Add 'Options.Applicative.help' to an option
  help :: String -> o attr a -> o attr a

instance HasHelp OptionOpt a where
  help s o = o { _oHelp = Just s }

instance HasHelp FlagOpt a where
  help s o = o { _fHelp = Just s }

instance HasHelp ArgumentOpt a where
  help s o = o { _aHelp = Just s }

class HasMetavar o (attr :: [OptAttr]) where
  -- | Add a 'Options.Applicative.metavar' metavar to an option, to be
  -- displayed as the meta-parameter next to long/short modifiers
  metavar :: String -> o attr a -> o attr a

instance HasMetavar OptionOpt a where
  metavar s o = o { _oMetavar = Just s }

instance HasMetavar ArgumentOpt a where
  metavar s o = o { _aMetavar = Just s }

class HasEnvVar o (attr :: [OptAttr]) where
  -- | Specify an environment variable to lookup for an option
  envVar :: String -> o attr a -> o attr a

instance HasEnvVar OptionOpt a where
  envVar s o = o { _oEnvVar = Just s }

instance HasEnvVar FlagOpt a where
  envVar s o = o { _fEnvVar = Just s }

instance HasEnvVar ArgumentOpt a where
  envVar s o = o { _aEnvVar = Just s }

class HasDefaultVal o (attr :: [OptAttr]) where
  -- | Add a default value to an option. Cannot be used in conjuction with
  -- with 'required', 'defaultStr' or 'optional'.
  defaultVal
    :: ( NotInAttrs OptDefault attr (DuplicateAttrMultipleErr "defaultVal" '["defaultStr", "required"])
       , NotInAttrs OptOptional attr (IncompatibleAttrsErr "defaultVal" "optional")
       )
    => a -> o attr a -> o (OptDefault ': attr) a

instance HasDefaultVal OptionOpt a where
  defaultVal a o = o { _oDefaultVal = Just a }

instance HasDefaultVal ArgumentOpt a where
  defaultVal a o = o { _aDefaultVal = Just a }

class HasDefaultStr o (attr :: [OptAttr]) where
  -- | Add a default unparsed value to an option. Cannot be used in conjuction
  -- with 'defaultVal', 'required' or 'optional'.
  defaultStr
    :: ( NotInAttrs OptDefault attr (DuplicateAttrMultipleErr "defaultStr" '["defaultVal", "required"])
       , NotInAttrs OptOptional attr (IncompatibleAttrsErr "defaultStr" "optional")
       )
    => String -> o attr a -> o (OptDefault ': attr) a

instance HasDefaultStr OptionOpt a where
  defaultStr s o = o { _oDefaultStr = Just s }

instance HasDefaultStr ArgumentOpt a where
  defaultStr s o = o { _aDefaultStr = Just s }

class HasRequired o (attr :: [OptAttr]) where
  -- | Mark an option as required. Cannot be used in conjunction with
  -- 'optional', 'defaultVal' or 'requiredStr'.
  required
    :: ( NotInAttrs OptDefault attr (DuplicateAttrMultipleErr "required" '["defaultVal", "defaultStr"])
       , NotInAttrs OptOptional attr (IncompatibleAttrsErr "required" "optional")
       )
    => o attr a -> o (OptDefault ': attr) a

instance HasRequired OptionOpt a where
  required o = o { _oDefaultVal = Nothing }

instance HasRequired ArgumentOpt a where
  required o = o { _aDefaultVal = Nothing }

-- | Class for options that can be optional. Cannot be used in conjunction with
-- 'HasDefaultVal', 'HasDefaultStr' or 'HasRequired'. Note that this will turn a
-- parser for @a@ into a parser for @Maybe a@, modifying the reader function
-- appropriately.
-- For example:
--
-- @
--   someOpt :: Opt (Maybe Int)
--   someOpt
--     = optionWith readParser
--         ( long "someopt"
--         . optional
--         )
-- @
class HasOptional o (attr :: [OptAttr]) where
  -- | Specify that an option is optional. This will convert an @Opt a@ to an
  -- @Opt (Maybe a)@. Cannot be used in conjunction with 'defaultVal', 'defaultStr'
  -- or 'required'.
  optional
    :: ( NotInAttrs OptOptional attr (DuplicateAttrErr "optional")
       , NotInAttrs OptDefault attr (IncompatibleAttrsErr "optional" "defaultVal")
       )
    => o attr a -> o (OptOptional ': attr) (Maybe a)

instance HasOptional OptionOpt a where
  optional OptionOpt{..}
    = OptionOpt
        { _oLong       = _oLong
        , _oShort      = _oShort
        , _oHelp       = _oHelp
        , _oMetavar    = _oMetavar
        , _oEnvVar     = _oEnvVar
        , _oDefaultVal = Just Nothing
        , _oDefaultStr = Nothing
        , _oReader     = fmap Just . _oReader
        }

instance HasOptional ArgumentOpt a where
  optional ArgumentOpt{..}
    = ArgumentOpt
        { _aHelp       = _aHelp
        , _aMetavar    = _aMetavar
        , _aEnvVar     = _aEnvVar
        , _aDefaultVal = Just Nothing
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
        , _optDefaultVal = _oDefaultVal
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
        , _optDefaultVal = Just _fDefaultVal
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
        , _optDefaultVal = _aDefaultVal
        , _optDefaultStr = _aDefaultStr
        , _optReader     = _aReader
        , _optType       = ArgumentOptType
        }

-- | Create an option parser, equivalent to 'Options.Applicative.option'. The
-- second argument is the modifiers to add to the option, and can be defined by
-- using function composition ('.').
--
-- @
--   someOption :: Opt Int
--   someOption
--     = option readParser
--         ( long "someopt"
--         . help "Some option"
--         . defaultVal 256
--         )
-- @
option
  :: OptReader a
  -> (OptionOpt '[] a -> OptionOpt attr b)
  -> Opt b
option p f
  = toOpt $ f opt
  where
    opt
      = OptionOpt
          { _oLong       = Nothing
          , _oShort      = Nothing
          , _oHelp       = Nothing
          , _oMetavar    = Nothing
          , _oEnvVar     = Nothing
          , _oDefaultVal = Nothing
          , _oDefaultStr = Nothing
          , _oReader     = p
          }

-- | Create a flag parser, equivalent to 'Options.Applicative.option'. The
-- first argument is the default value (returned when the flag modifier is
-- absent), and the second is the active value (returned when the flag
-- modifier is present). The second argument is the modifiers to add to the
-- option, and can be defined by using function composition ('.').
--
-- @
--   someFlag :: Opt Int
--   someFlag
--     = flag 0 1
--         ( long "someflag"
--         . help "Some flag"
--         )
-- @
flag
  :: a  -- ^ Default value
  -> a  -- ^ Active value
  -> (FlagOpt '[] a -> FlagOpt attr b)
  -> Opt b
flag d active f
  = toOpt $ f opt
  where
    opt
      = FlagOpt
          { _fLong       = Nothing
          , _fShort      = Nothing
          , _fHelp       = Nothing
          , _fEnvVar     = Nothing
          , _fDefaultVal = d
          , _fActive     = active
          , _fReader     = const (pure d)  -- TODO
          }

-- | A 'flag' parser, specialized to 'Bool'. The parser (e.g. when parsing
-- an environment variable) will accept @true@ and @false@, but case
-- insensitive, rather than using the 'Read' instance for 'Bool'. The
-- default value is 'False', and the active value is 'True'.
--
-- @
--   someSwitch :: Opt Bool
--   someSwitch
--     = switch
--         ( long "someswitch"
--         . help "Some switch"
--         )
-- @
switch
  :: (FlagOpt '[] Bool -> FlagOpt attr Bool)
  -> Opt Bool
switch f
  = fl { _optReader = boolParser }
  where
    fl
      = flag False True f

-- | Similar to 'switch', but the default value is 'True' and the active is
-- 'False'.
switch'
  :: (FlagOpt '[] Bool -> FlagOpt attr Bool)
  -> Opt Bool
switch' f
  = fl { _optReader = boolParser }
  where
    fl
      = flag True False f

-- | Create an argument parser, equivalent to 'Options.Applicative.argument'.
-- The second argument is the modifiers to add to the option, and can be
-- defined by using function composition ('.').
--
-- @
--   someArgument :: Opt Int
--   someArgument
--     = argument
--         ( help "Some argument"
--         . defaultVal "this is the default"
--         )
-- @
argument
  :: OptReader a
  -> (ArgumentOpt '[] a -> ArgumentOpt attr b)
  -> Opt b
argument p f
  = toOpt $ f opt
  where
    opt
      = ArgumentOpt
          { _aHelp       = Nothing
          , _aMetavar    = Nothing
          , _aEnvVar     = Nothing
          , _aDefaultVal = Nothing
          , _aDefaultStr = Nothing
          , _aReader     = p
          }

-- | Convert a parser that returns 'Maybe' to a parser that returns 'Either',
-- with the default 'Left' value @unable to parse: \<input\>@.
parseWith
  :: (String -> Maybe a)  -- ^ Original parser
  -> (String -> Either String a)
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

-- | A parser that can parse many items, returning a list.
manyParser
  :: String  -- ^ Separator
  -> OptReader a  -- ^ Parser for each string
  -> OptReader [a]
manyParser sep parser
  = traverse parser . splitOn sep

-- | Wrap a symbol in quotes, for pretty printing in type errors.
type QuoteSym (s :: Symbol)
  = 'Text "`" :<>: 'Text s :<>: 'Text "`"

-- | Check if `x` is not an element of the type-level list `xs`. If it is
-- print the appropriate error message using `l` and `r` for clarity.
type family NotInAttrs
    (x :: k)
    (xs :: [k])
    (err :: ErrorMessage)
    :: Constraint where
  NotInAttrs _ '[]  _
    = ()
  NotInAttrs x (x ': _) err
    = TypeError err
  NotInAttrs x (y ': xs) err
    = NotInAttrs x xs err

type family CommaSep (xs :: [Symbol]) :: Symbol where
  CommaSep '[] = ""
  CommaSep '[x] = " or " `AppendSymbol` x
  CommaSep (x ': xs) = " or one of " `AppendSymbol` CommaSep' x xs

type family CommaSep' (s :: Symbol) (xs :: [Symbol]) :: Symbol where
  CommaSep' s '[]       = s
  CommaSep' s (x ': xs) = CommaSep' (s `AppendSymbol` ", " `AppendSymbol` x) xs

type DuplicateAttrErr attr
  =    QuoteSym attr
  :<>: 'Text " is already specified."

type DuplicateAttrMultipleErr attr rest
  =    QuoteSym attr
  :<>: 'Text (CommaSep rest)
  :<>: 'Text " has already been specified."

type IncompatibleAttrsErr l r
  =    QuoteSym l
  :<>: 'Text " and "
  :<>: QuoteSym r
  :<>: 'Text " cannot be mixed in an option definition."
