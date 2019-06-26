{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Operations where

import           Data.Functor.Compose       (Compose (..))
import           Data.Functor.Identity      (Identity (..))
import           Data.Kind                  (Type)
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy                 (Proxy (..))
import           GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)
import qualified System.Environment         as Env

import qualified Data.Barbie                as B
import qualified Options.Applicative        as Optparse

import           Options.Harg.Het
import           Options.Harg.Het.AssocList
import           Options.Harg.Het.Nat
import           Options.Harg.Het.Variant
import           Options.Harg.Types

mkHelp
  :: Opt a
  -> String
mkHelp opt
  =  _optHelp opt
  <> maybe
       ""
       (\v -> " (env var: " <> v <> ")")
       (_optEnvVar opt)

getParser :: Opt a -> IO (Optparse.Parser a)
getParser opt@Opt{..} = do
  envVar <- case _optEnvVar of
    Nothing -> pure Nothing
    Just s  -> Env.lookupEnv s
  case _optType of
    ArgOptType -> do
      let
        parsedEnvVar = envVar >>= either (const Nothing) Just . _optParser
      pure
        $ Optparse.option (Optparse.eitherReader _optParser)
            ( foldMap (fromMaybe mempty)
                [ Just (Optparse.long _optLong)
                , Optparse.short <$> _optShort
                , Just (Optparse.help help)
                , Optparse.metavar <$> _optMetavar
                , Optparse.value <$> (parsedEnvVar Optparse.<|> _optDefault)
                ]
            )
    FlagOptType active -> do
      let
        def
          = case envVar of
              Nothing -> _optDefault
              Just _  -> Just active
        modifiers
          = foldMap (fromMaybe mempty)
              [ Just (Optparse.long _optLong)
              , Optparse.short <$> _optShort
              , Just (Optparse.help help)
              ]
      pure
        $ case def of
            Nothing ->
              Optparse.flag' active modifiers
            Just v ->
              Optparse.flag v active modifiers
  where
    help = mkHelp opt

getOpt
  :: B.TraversableB a
  => a Opt
  -> IO (a Identity)
getOpt opts = do
  -- get options from command line arguments
  parser <-
    getCompose
    $ B.btraverse
        (Compose <$> (fmap . fmap . fmap) Identity getParser)
        opts
  Optparse.execParser $
    Optparse.info (Optparse.helper <*> parser) mempty

getOptSubcommand
  :: forall xs ts.
     ( B.TraversableB (VariantF xs)
     , Subcommands Z ts xs '[]
     )
  => AssocListF ts xs Opt
  -> IO (VariantF xs Identity)
getOptSubcommand alist = do
  commands <- mapSubcommand @Z @ts @xs @'[] SZ alist

  -- get options from command line arguments using a subparser
  Optparse.execParser $
    Optparse.info (Optparse.helper <*> Optparse.subparser (mconcat commands)) mempty

-- subcommands
class Subcommands
    (n :: Nat)
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (acc :: [(Type -> Type) -> Type]) where
  mapSubcommand
    :: SNat n
    -> AssocListF ts xs Opt
    -> IO [Optparse.Mod Optparse.CommandFields (VariantF (acc ++ xs) Identity)]

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
  mapSubcommand n (ACons opt opts)
    = (:)
    <$> subcommand
    <*> hgcastWith  -- this applies the proof
        (proof @as @x @xs)  -- evidence
        (mapSubcommand @(S n) @ts @xs @(as ++ '[x]) (SS n) opts)
    where
      subcommand
        :: IO (Optparse.Mod Optparse.CommandFields (VariantF (as ++ (x ': xs)) Identity))
      subcommand
        = do
            parser <-
              getCompose
              $ B.btraverse
                  (Compose <$> (fmap . fmap . fmap) Identity getParser)
                  opt
            pure
              $ Optparse.command tag
              $ injectPosF n
                <$> Optparse.info (Optparse.helper <*> parser) mempty
      tag
        = symbolVal (Proxy :: Proxy t)

-- The following allows to use one function for commands + subcommands
type family OptOutput' a where
  OptOutput' (AssocListF ts xs) = VariantF xs Identity
  OptOutput' a = a Identity

class GetOpt a where
  type OptOutput a :: Type
  getOptions :: a -> IO (OptOutput a)

instance {-# OVERLAPPING #-}
         ( B.TraversableB (VariantF xs)
         , MapVariantF xs
         , Subcommands Z ts xs '[]
         ) => GetOpt (AssocListF ts xs Opt) where
  type OptOutput (AssocListF ts xs Opt) = OptOutput' (AssocListF ts xs)
  getOptions = getOptSubcommand

instance ( B.TraversableB a
         , OptOutput' a ~ a Identity
         ) => GetOpt (a Opt) where
  type OptOutput (a Opt) = OptOutput' a
  getOptions = getOpt
