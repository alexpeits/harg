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
  -> Optparse.Parser (a Identity)
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
       , Applicative f
       )
    => SNat n
    -> HList s
    -> AssocListF ts xs (Compose Opt f)
    -> [Optparse.Mod Optparse.CommandFields (VariantF (acc ++ xs) f)]

instance Subcommands n '[] '[] acc where
  mapSubcommand _ _ _ = []

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
    = let
        sc = subcommand
        rest = hgcastWith (proof @as @x @xs)
                          (mapSubcommand @(S n) @ts @xs @(as ++ '[x]) (SS n) srcs opts)
      in (sc : rest)

    where

      -- subcommand
        -- :: IO (Optparse.Mod Optparse.CommandFields (VariantF (as ++ (x ': xs)) Identity))
      subcommand
        = let
            (_err, src)
              = accumSourceResults $ runSource' srcs opt
            parser
              = mkOptparseParser src opt
            cmd
              = Optparse.command tag
              $ injectPosF n
              <$> Optparse.info (Optparse.helper <*> parser) mempty
          in cmd

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
