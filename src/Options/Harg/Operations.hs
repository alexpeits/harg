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

import           Options.Harg.Het.AssocList
import           Options.Harg.Het.Nat
import           Options.Harg.Het.Proofs
import           Options.Harg.Het.Variant
import           Options.Harg.Types

mkHelp
  :: Opt a
  -> String
mkHelp opt
  =  fromMaybe "" (_optHelp opt)
  <> maybe
       ""
       (\v -> " (env var: " <> v <> ")")
       (_optEnvVar opt)

data EnvVarParseResult a
  = EnvVarNotAvailable
  | EnvVarNotFound
  | EnvVarFoundNoParse String
  | EnvVarParsed a

getEnvVar :: EnvVarParseResult a -> Maybe a
getEnvVar
  = \case
      EnvVarParsed a -> Just a
      _              -> Nothing

tryParseEnvVar
  :: Opt a
  -> IO (EnvVarParseResult a)
tryParseEnvVar Opt{..}
  = case _optEnvVar of
      Nothing
        -> pure EnvVarNotAvailable
      Just envVar
        -> Env.lookupEnv envVar >>= pure . maybe EnvVarNotFound tryParse
  where
    tryParse
      = either EnvVarFoundNoParse EnvVarParsed . _optParser

toParser :: Opt a -> IO (Parser a)
toParser opt@Opt{..} = do
  envVarRes <- tryParseEnvVar opt

  case _optType of

    ArgOptType -> do
      let
        parsedEnvVar
          = getEnvVar envVarRes
        err
          = case envVarRes of
              EnvVarFoundNoParse detail
                -> [OptError (SomeOpt opt) detail]
              _
                -> []
        option
          = Optparse.option (Optparse.eitherReader _optParser)
              ( foldMap (fromMaybe mempty)
                  [ Optparse.long <$> _optLong
                  , Optparse.short <$> _optShort
                  , Just (Optparse.help help)
                  , Optparse.metavar <$> _optMetavar
                  , Optparse.value <$> (parsedEnvVar Optparse.<|> _optDefault)
                  ]
              )

      pure $ Parser option err

    FlagOptType active -> do
      let
        mDef
          = case getEnvVar envVarRes of
              Nothing -> _optDefault
              Just _  -> Just active
        modifiers
          = foldMap (fromMaybe mempty)
              [ Optparse.long <$> _optLong
              , Optparse.short <$> _optShort
              , Just (Optparse.help help)
              ]
        option
          = case mDef of
              Nothing ->
                Optparse.flag' active modifiers
              Just def ->
                Optparse.flag def active modifiers

      pure $ Parser option []

  where
    help = mkHelp opt

getOptParser
  :: B.TraversableB a
  => a Opt
  -> IO (Parser (a Identity))
getOptParser opts =
  getCompose
    $ B.btraverse (Compose <$> (fmap . fmap . fmap) Identity toParser) opts

getOptParserSubcommand
  :: forall xs ts.
     ( B.TraversableB (VariantF xs)
     , Subcommands Z ts xs '[]
     )
  => AssocListF ts xs Opt
  -> IO (Parser (VariantF xs Identity))
getOptParserSubcommand alist = do
  (commands, err) <- mapSubcommand @Z @ts @xs @'[] SZ alist

  let
    parser
      = Optparse.subparser (mconcat commands)

  pure $ Parser parser err

-- subcommands
class Subcommands
    (n :: Nat)
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (acc :: [(Type -> Type) -> Type]) where
  mapSubcommand
    :: SNat n
    -> AssocListF ts xs Opt
    -> IO ([Optparse.Mod Optparse.CommandFields (VariantF (acc ++ xs) Identity)], [OptError])

instance Subcommands n '[] '[] acc where
  mapSubcommand _ _ = pure ([], [])

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
    = do
        (sc, err) <- subcommand
        (rest, errs) <- hgcastWith  -- this applies the proof
                          (proof @as @x @xs)  -- evidence
                          (mapSubcommand @(S n) @ts @xs @(as ++ '[x]) (SS n) opts)
        pure (sc : rest, err <> errs)
    where
      subcommand
        :: IO (Optparse.Mod Optparse.CommandFields (VariantF (as ++ (x ': xs)) Identity), [OptError])
      subcommand
        = do
            (Parser parser err) <-
              getCompose
              $ B.btraverse
                  (Compose <$> (fmap . fmap . fmap) Identity toParser)
                  opt
            let cmd
                  = Optparse.command tag
                  $ injectPosF n
                  <$> Optparse.info (Optparse.helper <*> parser) mempty
            pure (cmd, err)
      tag
        = symbolVal (Proxy :: Proxy t)

-- The following allows to use one function for commands + subcommands
type family OptResult' a where
  OptResult' (AssocListF ts xs) = VariantF xs Identity
  OptResult' a = a Identity

class GetParser a where
  type OptResult a :: Type
  getParser :: a -> IO (Parser (OptResult a))

instance {-# OVERLAPPING #-}
         ( B.TraversableB (VariantF xs)
         , Subcommands Z ts xs '[]
         ) => GetParser (AssocListF ts xs Opt) where
  type OptResult (AssocListF ts xs Opt) = OptResult' (AssocListF ts xs)
  getParser = getOptParserSubcommand

instance ( B.TraversableB a
         , OptResult' a ~ a Identity
         ) => GetParser (a Opt) where
  type OptResult (a Opt) = OptResult' a
  getParser = getOptParser
