{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Operations where

import           Data.Functor.Compose       (Compose (..))
import           Data.Kind                  (Type)
import           Data.Maybe                 (catMaybes)
import           Data.Proxy                 (Proxy (..))
import           GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)
import qualified System.Environment         as Env

import qualified Data.Barbie                as B
import qualified Options.Applicative        as Args

import           Options.Harg.Het
import           Options.Harg.Het.All
import           Options.Harg.Het.AssocList
import           Options.Harg.Het.Nat
import           Options.Harg.Het.Variant
import           Options.Harg.Pretty
import           Options.Harg.Types


parseOpt :: Opt a -> String -> OptValue a
parseOpt opt@Opt{..}
  = either (toOptInvalid opt) pure
  . _optParser

fromCmdLine :: Opt a -> IO (Args.Parser (OptValue a))
fromCmdLine opt@Opt{..} = do
  envVar <- case _optEnvVar of
    Nothing -> pure Nothing
    Just s  -> Env.lookupEnv s
  case _optType of
    ArgOptType -> do
      let
        mParser = either (const Nothing) Just . _optParser
        parsedEnvVar = envVar >>= mParser
      pure
        $ pure
        <$> Args.option (Args.maybeReader mParser)
              ( mconcat . catMaybes
              $ [ Just (Args.long _optLong)
                , Args.short <$> _optShort
                , Just (Args.help help)
                , Args.metavar <$> _optMetavar
                , Args.value <$> (parsedEnvVar Args.<|> _optDefault)
                ]
              )
    FlagOptType active -> do
      let
        def
          = case envVar of
              Nothing -> _optDefault
              Just _  -> Just active
        modifiers
          = mconcat . catMaybes
          $ [ Just (Args.long _optLong)
            , Args.short <$> _optShort
            , Just (Args.help help)
            ]
      pure
        $ pure
        <$> case def of
              Nothing ->
                 Args.flag' active modifiers
              Just v ->
                 Args.flag v active modifiers
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
  :: ( B.TraversableB a
     , Semigroup (a OptValue)
     )
  => a Opt
  -> IO (a OptValue)
getOpt opts = do
  -- get options from command line arguments
  parser <- getCompose $ B.btraverse (Compose <$> fromCmdLine) opts
  Args.execParser $
    Args.info (Args.helper <*> parser) mempty

getOptSubcommand
  :: forall xs ts.
     ( B.TraversableB (VariantF xs)
     , Semigroup (VariantF xs OptValue)
     , MapVariantF xs
     , Subcommands Z ts xs '[]
     )
  => AssocListF ts xs Opt
  -> IO (VariantF xs OptValue)
getOptSubcommand alist = do
  commands <- mapSubcommand @Z @ts @xs @'[] SZ alist

  -- get options from command line arguments using a subparser
  Args.execParser $
    Args.info (Args.helper <*> Args.subparser (mconcat commands)) mempty

-- subcommands
class Subcommands
    (n :: Nat)
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (acc :: [(Type -> Type) -> Type]) where
  mapSubcommand
    :: SNat n
    -> AssocListF ts xs Opt
    -> IO [Args.Mod Args.CommandFields (VariantF (acc ++ xs) OptValue)]

instance Subcommands n '[] '[] acc where
  mapSubcommand _ _ = pure []

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
    = (:)
    <$> subcommand
    <*> hgcastWith  -- this applies the proof
        (proof @as @x @xs)  -- evidence
        (mapSubcommand @(S n) @ts @xs @(as ++ '[x]) (SS n) xs)
    where
      subcommand
        :: IO (Args.Mod Args.CommandFields (VariantF (as ++ (x ': xs)) OptValue))
      subcommand
        = do
            parser <- getCompose $ B.btraverse (Compose <$> fromCmdLine) x
            pure
              $ Args.command tag
              $ injectPosF n
                <$> Args.info (Args.helper <*> parser) mempty
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

instance ( B.TraversableB a
         , Semigroup (a OptValue)
         , OptOutput' a ~ a OptValue
         ) => GetOpt (a Opt) where
  type OptOutput (a Opt) = OptOutput' a
  getOptions = getOpt
