{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Subcommands where

import           Data.Functor.Compose       (Compose (..))
import           Data.Kind                  (Type)
import           Data.Proxy                 (Proxy (..))
import           GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)

import qualified Data.Barbie                as B
import qualified Options.Applicative        as Optparse

import           Options.Harg.Cmdline
import           Options.Harg.Het.All
import           Options.Harg.Het.HList
import           Options.Harg.Het.Nat
import           Options.Harg.Het.Proofs
import           Options.Harg.Het.Variant
import           Options.Harg.Sources
import           Options.Harg.Sources.Types
import           Options.Harg.Types

class Subcommands
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type]) where
  mapSubcommand
    :: ( All (RunSource s) xs
       , Applicative f
       )
    => HList s
    -> AssocListF ts xs (Compose Opt f)
    -> [Optparse.Mod Optparse.CommandFields (VariantF xs f)]

instance ExplSubcommands Z ts xs '[] => Subcommands ts xs where
  mapSubcommand = explMapSubcommand @Z @ts @xs @'[] SZ


class ExplSubcommands
    (n :: Nat)
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (acc :: [(Type -> Type) -> Type]) where
  explMapSubcommand
    :: ( All (RunSource s) xs
       , Applicative f
       )
    => SNat n
    -> HList s
    -> AssocListF ts xs (Compose Opt f)
    -> [Optparse.Mod Optparse.CommandFields (VariantF (acc ++ xs) f)]

instance ExplSubcommands n '[] '[] acc where
  explMapSubcommand _ _ _ = []

-- ok wait
-- hear me out:
instance ( ExplSubcommands (S n) ts xs (as ++ '[x])
         -- get the correct injection into the variant by position
         , InjectPosF n x (as ++ (x ': xs))
         , B.TraversableB x
         , B.ProductB x
         , KnownSymbol t
         -- prove that xs ++ (y : ys) ~ (xs ++ [y]) ++ ys
         , Proof as x xs
         ) => ExplSubcommands n (t ': ts) (x ': xs) as where

  explMapSubcommand n srcs (ACons opt opts)
    = let
        sc
          = subcommand
        rest
          = hgcastWith (proof @as @x @xs)
          $ explMapSubcommand
              @(S n) @ts @xs @(as ++ '[x])
              (SS n) srcs opts

      in (sc : rest)

    where
      subcommand
        = let
            -- TODO: accumulate errors
            (_err, src)
              = accumSourceResults $ runSource srcs opt
            parser
              = mkOptparseParser src opt
            tag
              = symbolVal (Proxy :: Proxy t)
            cmd
              = Optparse.command tag
              $ injectPosF n
              <$> Optparse.info (Optparse.helper <*> parser) mempty
          in cmd
