{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Operations where

import           Data.Kind                    (Type)
import           Data.Maybe                   (catMaybes)
import           Data.Proxy                   (Proxy(..))
import           GHC.TypeLits                 (Symbol, KnownSymbol, symbolVal)
import qualified System.Environment           as Env

import qualified Data.Barbie                  as B
import qualified Options.Applicative          as Args

import           Options.Harg.Het
import           Options.Harg.Het.All
import           Options.Harg.Het.AssocList
import           Options.Harg.Het.Nat
import           Options.Harg.Het.Variant
import           Options.Harg.Pretty
import           Options.Harg.Types

import Data.Functor.Compose
import Control.Applicative (Alternative(..))

parseOpt :: Opt a -> String -> OptValue a
parseOpt opt@Opt{..}
  = either (toOptInvalid opt) pure
  . _optParser

fromCmdLine' :: Opt a -> IO (Args.Parser (OptValue a))
fromCmdLine' opt@Opt{..}
  = case _optType of
      ArgOptType -> do
        envVar <- case _optEnvVar of
          Nothing -> pure Nothing
          Just s  -> Env.lookupEnv s
        let
          mParser = either (const Nothing) Just . _optParser
          parsedEnvVar = envVar >>= mParser
        pure
          $ pure
          <$>
                ( Args.option (Args.maybeReader mParser)
                    ( Args.long _optLong
                    <> maybe mempty Args.short _optShort
                    <> Args.help help
                    <> maybe mempty Args.metavar _optMetavar
                    <> maybe mempty Args.value (parsedEnvVar <|> _optDefault)
                    )
                -- $ mconcat . catMaybes
                -- $ [ Just (Args.long _optLong)
                  -- , Args.short <$> _optShort
                  -- , Just (Args.help help)
                  -- , Args.metavar <$> _optMetavar
                  -- , Args.value <$> (parsedEnvVar Args.<|> _optDefault)
                  -- ]
                )
      FlagOptType active -> do
        envVar <- case _optEnvVar of
          Nothing -> pure Nothing
          Just s  -> Env.lookupEnv s
        let
          def = case envVar of
            Nothing -> _optDefault
            Just _ -> Just active
        pure
          $ pure
          <$> case def of
                Nothing ->
                  ( Args.flag' active -- (Args.maybeReader mParser)
                      ( Args.long _optLong
                      <> maybe mempty Args.short _optShort
                      <> Args.help help
                      )
                  )
                Just v ->
                  ( Args.flag v active -- (Args.maybeReader mParser)
                      ( Args.long _optLong
                      <> maybe mempty Args.short _optShort
                      <> Args.help help
                      )
                  )
                -- $ mconcat . catMaybes
                -- $ [ Just (Args.long _optLong)
                  -- , Args.short <$> _optShort
                  -- , Just (Args.help help)
                  -- , Args.metavar <$> _optMetavar
                  -- , Args.value <$> (parsedEnvVar Args.<|> _optDefault)
                  -- ]
        -- pure
          -- $ maybe (toOptNotPresent opt) pure
          -- <$> Args.optional
                -- ( Args.flag' active
                -- $ mconcat . catMaybes
                -- $ [ Just (Args.long _optLong)
                  -- , Args.short <$> _optShort
                  -- , Just (Args.help help)
                  -- ]
                -- )
  where
    help = mkHelp opt

fromCmdLine :: Opt a -> Args.Parser (OptValue a)
fromCmdLine opt@Opt{..}
  = case _optType of
      ArgOptType ->
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
      FlagOptType active ->
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
  :: ( B.TraversableB a
     , Semigroup (a OptValue)
     )
  => a Opt
  -> IO (a OptValue)
getOpt opts = do
  -- get options from command line arguments
  ioParser <- getCompose $ B.btraverse @_ @_ @(Compose IO Args.Parser) (Compose <$> fromCmdLine') opts
  aOpt <- Args.execParser $
    Args.info (Args.helper <*> ioParser) mempty

  -- get options from environment variables
  -- eOpt <- B.btraverse fromEnvVar opts

  -- get options from defaults
  -- let dOpt = B.bmap fromDefault opts

  pure aOpt
  -- pure (aOpt <> eOpt <> dOpt)

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

instance ( B.TraversableB a
         , Semigroup (a OptValue)
         , OptOutput' a ~ a OptValue
         ) => GetOpt (a Opt) where
  type OptOutput (a Opt) = OptOutput' a
  getOptions = getOpt
