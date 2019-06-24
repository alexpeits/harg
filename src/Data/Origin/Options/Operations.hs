{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Origin.Options.Operations where

import           Data.Kind                    (Type)
import           Data.Maybe                   (catMaybes)
import           Data.Proxy                   (Proxy(..))
import           GHC.TypeLits                 (Symbol, KnownSymbol, symbolVal)
import qualified System.Environment           as Env

import qualified Data.Barbie                  as B
import qualified Options.Applicative          as Args

import           Data.Origin.Het
import           Data.Origin.Het.All
import           Data.Origin.Het.AssocList
import           Data.Origin.Het.Nat
import           Data.Origin.Het.Variant
import           Data.Origin.Options.Pretty
import           Data.Origin.Options.Types

class Short (o :: Type -> Type) where
  optShort :: Char -> o a -> o a
  getShort :: o a -> Maybe Char
  toShortOpt :: o a -> Maybe Char

instance Short ArgOpt where
  optShort c o = o { _aShort = Just c }
  getShort = _aShort
  toShortOpt = _aShort

instance Short SwitchOpt where
  optShort c o = o { _sShort = Just c }
  getShort = _sShort
  toShortOpt = _sShort

class Help (o :: Type -> Type) where
  optHelp :: String -> o a -> o a
  getHelp :: o a -> String
  toHelpOpt :: o a -> String

instance Help ArgOpt where
  optHelp s o = o { _aHelp = s }
  getHelp = _aHelp
  toHelpOpt = _aHelp

instance Help SwitchOpt where
  optHelp s o = o { _sHelp = s }
  getHelp = _sHelp
  toHelpOpt = _sHelp

class Metavar (o :: Type -> Type) where
  optMetavar :: String -> o a -> o a
  getMetavar :: o a -> Maybe String
  toMetavarOpt :: o a -> Maybe String

instance Metavar ArgOpt where
  optMetavar s o = o { _aMetavar = Just s }
  getMetavar = _aMetavar
  toMetavarOpt = _aMetavar

instance Metavar SwitchOpt where
  optMetavar s o = o { _sMetavar = Just s }
  getMetavar = _sMetavar
  toMetavarOpt = _sMetavar

class EnvVar (o :: Type -> Type) where
  optEnvVar :: String -> o a -> o a
  getEnvVar :: o a -> Maybe String
  toEnvVarOpt :: o a -> Maybe String

instance EnvVar ArgOpt where
  optEnvVar s o = o { _aEnvVar = Just s }
  getEnvVar = _aEnvVar
  toEnvVarOpt = _aEnvVar

instance EnvVar SwitchOpt where
  optEnvVar s o = o { _sEnvVar = Just s }
  getEnvVar = _sEnvVar
  toEnvVarOpt = _sEnvVar

class Def (o :: Type -> Type) where
  type DefType o a :: Type
  optDefault :: a -> o a -> o a
  getDef :: o a -> DefType o a
  toDefOpt :: o a -> Maybe a

instance Def ArgOpt where
  type DefType ArgOpt a = Maybe a
  optDefault a o = o { _aDefault = Just a }
  getDef = _aDefault
  toDefOpt = _aDefault

class Parser (o :: Type -> Type) where
  getParser :: o a -> (String -> Either String a)


class ToOpt (o :: Type -> Type) where
  mkOpt :: o a -> Opt a

instance ToOpt ArgOpt where
  mkOpt MkArgOpt{..}
    = Opt
        _aLong
        _aShort
        _aHelp
        _aMetavar
        _aEnvVar
        _aDefault
        _aParser
        ArgOpt

instance ToOpt SwitchOpt where
  mkOpt MkSwitchOpt{..}
    = Opt
        _sLong
        _sShort
        _sHelp
        _sMetavar
        _sEnvVar
        (Just _sDefault)
        _sParser
        (FlagOpt _sActive)

-- constructing options
option
  :: String
  -> (String -> Either String a)
  -> ArgOpt a
option long p
  = MkArgOpt
      { _aLong    = long
      , _aShort   = Nothing
      , _aHelp    = ""
      , _aMetavar = Nothing
      , _aEnvVar  = Nothing
      , _aDefault = Nothing
      , _aParser  = p
      }

flag
  :: String
  -> a
  -> a
  -> SwitchOpt a
flag long d active
  = MkSwitchOpt
      { _sLong    = long
      , _sShort   = Nothing
      , _sHelp    = ""
      , _sMetavar = Nothing
      , _sEnvVar  = Nothing
      , _sDefault = d
      , _sActive  = active
      , _sParser  = const (pure d)
      }

switch :: String -> SwitchOpt Bool
switch long
  = flag long False True

switch' :: String -> SwitchOpt Bool
switch' long
  = flag long True False

-- optShort :: Char -> Opt a -> Opt a
-- optShort c opt
--   = opt { _optShort = Just c }

-- optHelp :: String -> Opt a -> Opt a
-- optHelp s opt
--   = opt { _optHelp = s }

-- optMetavar :: String -> Opt a -> Opt a
-- optMetavar s opt
--   = opt { _optMetavar = Just s }

-- optEnvVar :: String -> Opt a -> Opt a
-- optEnvVar s opt
--   = opt { _optEnvVar = Just s }

-- optDefault :: a -> Opt a -> Opt a
-- optDefault d opt
--   = opt { _optDefault = Just d }

-- parsing and fetching options
parseOpt :: Opt a -> String -> OptValue a
parseOpt opt@Opt{..}
  = either (toOptInvalid opt) pure
  . _optParser

fromCmdLine :: Opt a -> Args.Parser (OptValue a)
fromCmdLine opt@Opt{..}
  = case _optType of
      ArgOpt ->
        maybe (toOptNotPresent opt) (parseOpt opt)
        <$> Args.optional
              ( Args.strOption
              $ mconcat . catMaybes
              $ [ Just (Args.long _optLong)
                , Args.short <$> _optShort
                , Just (Args.help help)
                , Args.metavar <$> _optMetavar
                ]
              )
      FlagOpt active ->
        maybe (toOptNotPresent opt) pure
        <$> Args.optional
              ( Args.flag' active
              $ mconcat . catMaybes
              $ [ Just (Args.long _optLong)
                , Args.short <$> _optShort
                , Just (Args.help help)
                ]
              )
  where
    help = mkHelp opt

fromEnvVar :: Opt a -> IO (OptValue a)
fromEnvVar opt
  = maybe (toOptNotPresent opt) (parseOpt opt)
  <$> maybe (pure Nothing) Env.lookupEnv (_optEnvVar opt)

fromDefault :: Opt a -> OptValue a
fromDefault opt
  = maybe
      (toOptNotPresent opt)
      pure
      (_optDefault opt)

getOpt
  :: ( B.FunctorB a
     , B.TraversableB a
     , Semigroup (a OptValue)
     )
  => a Opt
  -> IO (a OptValue)
getOpt opts = do
  -- get options from command line arguments
  aOpt <- Args.execParser $
    Args.info (Args.helper <*> B.btraverse fromCmdLine opts) mempty

  -- get options from environment variables
  eOpt <- B.btraverse fromEnvVar opts

  -- get options from defaults
  let dOpt = B.bmap fromDefault opts

  pure (aOpt <> eOpt <> dOpt)

getOptSubcommand
  :: forall xs ts.
     ( B.TraversableB (VariantF xs)
     , AllF Semigroup xs OptValue
     , Semigroup (VariantF xs OptValue)
     , MapVariantF xs
     , Subcommands Z ts xs '[]
     )
  => AssocListF ts xs Opt
  -> IO (VariantF xs OptValue)
getOptSubcommand alist = do
  let commands = mapSubcommand @Z @ts @xs @'[] SZ alist

  -- get options from command line arguments using a subparser
  aOpt <- Args.execParser $
    Args.info (Args.helper <*> Args.subparser (mconcat commands)) mempty

  -- find out which options were actually selected by looking at the
  -- resulting type of aOpt
  let opts = mapVariantF aOpt $ assocToHListF alist

  -- get options from environment variables
  eOpt <- B.btraverse fromEnvVar opts

  -- get options from defaults
  let dOpt = B.bmap fromDefault opts

  pure $ aOpt <> eOpt <> dOpt

-- subcommands
class Subcommands
    (n :: Nat)
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (acc :: [(Type -> Type) -> Type]) where
  mapSubcommand
    :: SNat n
    -> AssocListF ts xs Opt
    -> [Args.Mod Args.CommandFields (VariantF (acc ++ xs) OptValue)]

instance Subcommands n '[] '[] acc where
  mapSubcommand _ _ = []

-- ok wait
-- hear me out:
instance ( Subcommands (S n) ts xs (as ++ '[x])
         -- get the correct injection into the variant by position
         , InjectPosF n x (as ++ (x ': xs))
         , B.TraversableB x
         , KnownSymbol t
         -- prove that xs ++ (y : ys) ~ (xs ++ [y]) ++ ys
         , Proof as x xs
         ) => Subcommands n (t ': ts) (x ': xs) as where
  mapSubcommand n (ACons x xs)
    = subcommand
    : hgcastWith  -- this applies the proof
        (proof @as @x @xs)  -- evidence
        (mapSubcommand @(S n) @ts @xs @(as ++ '[x]) (SS n) xs)
    where
      subcommand
        :: Args.Mod Args.CommandFields (VariantF (as ++ (x ': xs)) OptValue)
      subcommand
        = Args.command tag
        $ injectPosF n
          <$> Args.info (Args.helper <*> B.btraverse fromCmdLine x) mempty
      tag
        = symbolVal (Proxy :: Proxy t)

-- The following allows to use one function for commands + subcommands
type family OptOutput' a where
  OptOutput' (AssocListF ts xs) = VariantF xs OptValue
  OptOutput' a = a OptValue

class GetOpt a where
  type OptOutput a :: Type
  getOptions :: a -> IO (OptOutput a)

instance {-# OVERLAPPING #-}
         ( B.TraversableB (VariantF xs)
         , AllF Semigroup xs OptValue
         , Semigroup (VariantF xs OptValue)
         , MapVariantF xs
         , Subcommands Z ts xs '[]
         ) => GetOpt (AssocListF ts xs Opt) where
  type OptOutput (AssocListF ts xs Opt) = OptOutput' (AssocListF ts xs)
  getOptions = getOptSubcommand

instance ( B.FunctorB a
         , B.TraversableB a
         , Semigroup (a OptValue)
         , OptOutput' a ~ a OptValue
         ) => GetOpt (a Opt) where
  type OptOutput (a Opt) = OptOutput' a
  getOptions = getOpt
