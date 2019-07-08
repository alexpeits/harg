{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Parser where

import           Data.Functor.Compose     (Compose (..))
import           Data.Functor.Identity    (Identity (..))
import           Data.Kind                (Type)
import           Data.Proxy               (Proxy (..))
import           GHC.TypeLits             (KnownSymbol, Symbol, symbolVal)

import qualified Data.Barbie              as B
import qualified Options.Applicative      as Optparse

import           Options.Harg.Cmdline
import           Options.Harg.Het.All
import           Options.Harg.Het.HList
import           Options.Harg.Het.Nat
import           Options.Harg.Het.Proofs
import           Options.Harg.Het.Variant
import           Options.Harg.Sources
import           Options.Harg.Types
import           Options.Harg.Util

getOptParser
  :: forall a.
     ( B.TraversableB a
     , B.ProductB a
     , B.FunctorB a
     )
  => [a Maybe]
  -> a Opt
  -> IO (Optparse.Parser (a Identity))
getOptParser sources opts
  = mkOptparseParser
      (fmap (compose Identity) sources)
      (compose Identity opts)

-- getOptParserSubcommand
  -- :: forall xs ts.
     -- ( B.TraversableB (VariantF xs)
     -- , Subcommands Z ts xs '[]
     -- )
  -- => [a Maybe]
  -- -> AssocListF ts xs Opt
  -- -> IO (Parser (VariantF xs Identity))
-- getOptParserSubcommand sources alist
  -- = do
      -- (commands, err)
        -- <- mapSubcommand @Z @ts @xs @'[] SZ sources alist
      -- let
        -- parser
          -- = Optparse.subparser (mconcat commands)
      -- pure $ Parser parser err

-- subcommands
class Subcommands
    (n :: Nat)
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (acc :: [(Type -> Type) -> Type]) where
  mapSubcommand
    :: ( All (RunSource s) xs
       )
    => SNat n
    -> HList s
    -> AssocListF ts xs Opt
    -> IO [Optparse.Mod Optparse.CommandFields (VariantF (acc ++ xs) Identity)]

instance Subcommands n '[] '[] acc where
  mapSubcommand _ _ _ = pure []

-- ok wait
-- hear me out:
instance ( Subcommands (S n) ts xs (as ++ '[x])
         -- get the correct injection into the variant by position
         , InjectPosF n x (as ++ (x ': xs))
         , B.TraversableB x
         , B.ProductB x
         , KnownSymbol t
         -- prove that xs ++ (y : ys) ~ (xs ++ [y]) ++ ys
         , Proof as x xs
         ) => Subcommands n (t ': ts) (x ': xs) as where

  mapSubcommand n srcs (ACons opt opts)
    = do
        sc <- subcommand
        rest <- hgcastWith (proof @as @x @xs)
                          (mapSubcommand @(S n) @ts @xs @(as ++ '[x]) (SS n) srcs opts)
        pure (sc : rest)

    where

      subcommand
        :: IO (Optparse.Mod Optparse.CommandFields (VariantF (as ++ (x ': xs)) Identity))
      subcommand
        = do
            let
              (_err, src) = accumSourceResults $ runSource' srcs opt
            parser
              <- mkOptparseParser
                   (fmap (compose Identity) src)
                   (compose Identity opt)
            let
              cmd
                = Optparse.command tag
                $ injectPosF n
                <$> Optparse.info (Optparse.helper <*> parser) mempty
            pure cmd

      tag
        = symbolVal (Proxy :: Proxy t)

-- subcommands
class DummySubcommands
    (n :: Nat)
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (acc :: [(Type -> Type) -> Type]) where
  mapDummySubcommand
    :: Applicative f
    => SNat n
    -> AssocListF ts xs (Compose Opt f)
    -> IO [Optparse.Mod Optparse.CommandFields (VariantF (acc ++ xs) f)]

instance DummySubcommands n '[] '[] acc where
  mapDummySubcommand _ _ = pure []

-- ok wait
-- hear me out:
instance ( DummySubcommands (S n) ts xs (as ++ '[x])
         -- get the correct injection into the variant by position
         , InjectPosF n x (as ++ (x ': xs))
         , B.TraversableB x
         , B.ProductB x
         , KnownSymbol t
         -- prove that xs ++ (y : ys) ~ (xs ++ [y]) ++ ys
         , Proof as x xs
         ) => DummySubcommands n (t ': ts) (x ': xs) as where

  mapDummySubcommand n (ACons opt opts)
    = do
        sc <- subcommand
        rest <- hgcastWith (proof @as @x @xs)
                          (mapDummySubcommand @(S n) @ts @xs @(as ++ '[x]) (SS n) opts)
        pure (sc : rest)

    where

      -- subcommand
        -- :: IO (Optparse.Mod Optparse.CommandFields (VariantF (as ++ (x ': xs)) f))
      subcommand
        = do
            parser
              <- mkOptparseParser
                   []
                   opt
            let
              cmd
                = Optparse.command tag
                $ injectPosF n
                <$> Optparse.info (Optparse.helper <*> parser) mempty
            pure cmd

      tag
        = symbolVal (Proxy :: Proxy t)

-- The following allows to use one function for commands + subcommands
type family OptResult' a where
  OptResult' (AssocListF ts xs) = VariantF xs Identity
  OptResult' a = a Identity

-- class GetParser a where
  -- type OptResult a :: Type
  -- getParser :: [a Maybe] -> a -> IO (Parser (OptResult a))

-- instance {-# OVERLAPPING #-}
         -- ( B.TraversableB (VariantF xs)
         -- , Subcommands Z ts xs '[]
         -- ) => GetParser (AssocListF ts xs Opt) where
  -- type OptResult (AssocListF ts xs Opt) = OptResult' (AssocListF ts xs)
  -- getParser = getOptParserSubcommand

-- instance ( B.TraversableB a, B.ProductB a
--          , OptResult' a ~ a Identity
--          ) => GetParser (a Opt) where
--   type OptResult (a Opt) = OptResult' a
--   getParser = getOptParser
