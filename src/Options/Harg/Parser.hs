{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Parser where

import           Control.Applicative        ((<|>))
import           Data.Functor.Compose       (Compose (..))
import           Data.Functor.Identity      (Identity (..))
import           Data.Kind                  (Type)
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy                 (Proxy (..))
import           GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)

import qualified Data.Barbie                as B
import qualified Options.Applicative        as Optparse

import           Options.Harg.Env
import           Options.Harg.Help
import           Options.Harg.Het.AssocList
import           Options.Harg.Het.Nat
import           Options.Harg.Het.Proofs
import           Options.Harg.Het.Variant
import           Options.Harg.Types


toParser :: Opt a -> IO (Parser a)
toParser opt@Opt{..} = do
  envVarRes <- tryParseEnvVar opt

  let
    parsedEnvVar
      = getEnvVar envVarRes
    err
      = case envVarRes of
          EnvVarFoundNoParse detail
            -> [OptError (SomeOpt opt) detail]
          _
            -> []

  case _optType of

    OptionOptType -> do
      let
        optionOpt
          = Optparse.option (Optparse.eitherReader _optReader)
              ( foldMap (fromMaybe mempty)
                  [ Optparse.long <$> _optLong
                  , Optparse.short <$> _optShort
                  , Optparse.help <$> help
                  , Optparse.metavar <$> _optMetavar
                  , Optparse.value <$> (parsedEnvVar <|> _optDefault)
                  ]
              )

      pure $ Parser optionOpt err

    FlagOptType active -> do
      let
        mDef
          = case parsedEnvVar of
              Nothing -> _optDefault
              Just _  -> Just active
        modifiers
          = foldMap (fromMaybe mempty)
              [ Optparse.long <$> _optLong
              , Optparse.short <$> _optShort
              , Optparse.help <$> help
              ]
        flagOpt
          = case mDef of
              Nothing ->
                Optparse.flag' active modifiers
              Just def ->
                Optparse.flag def active modifiers

      pure $ Parser flagOpt []

    ArgumentOptType -> do
      let
        argOpt
          = Optparse.argument (Optparse.eitherReader _optReader)
              ( foldMap (fromMaybe mempty)
                  [ Optparse.help <$> help
                  , Optparse.metavar <$> _optMetavar
                  , Optparse.value <$> (parsedEnvVar <|> _optDefault)
                  ]
              )

      pure $ Parser argOpt err

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
        (rest, errs) <- hgcastWith
                          (proof @as @x @xs)
                          (mapSubcommand @(S n) @ts @xs @(as ++ '[x]) (SS n) opts)
        pure (sc : rest, err <> errs)

    where

      subcommand
        :: IO ( Optparse.Mod Optparse.CommandFields (VariantF (as ++ (x ': xs)) Identity)
              , [OptError]
              )
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
