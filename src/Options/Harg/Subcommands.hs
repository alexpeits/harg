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

-- | This class can be used with an 'AssocList'. It returns the appropriate
-- list of 'Optparse.CommandFields' in order to create a subcommand parser.
-- Given the sources to use and the association list between the command string
-- and the command type, it returns the list of command field modifiers and a
-- list of errors.
--
-- The result can be used as follows:
--
-- @
--   ...
--   (errs, commands) = 'mapSubcommand' sources opts
--   parser = 'Optparse.subparser' ('mconcat' commands)
--   ...
-- @
--
-- In order to be able to create a subcommand parser for a heterogeneous list
-- of options (rather than a sum with different constructors), the return type
-- should also be heterogeneous. Here, we return a Variant, which is a more
-- generic version of 'Either'. In order to do that, 'mapSubcommand' traverses
-- the association list and creates an injection into the Variant, according to
-- the current position. So an 'AssocList' like this:
--
-- @
--   opts :: AssocList '["run", "test"] '[RunConfig, TestConfig] Opt
--   opts = ...
-- @
--
-- Should return @VariantF '[RunConfig, TestConfig] Identity@. In order to do
-- that, it will inject @RunConfig@ based on its position (0) using @HereF@,
-- and @TestConfig@ using @ThereF . HereF@ because its position is 1.
--
class Subcommands
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type]) where
  mapSubcommand
    :: ( All (RunSource s) xs
       , Applicative f
       )
    => s
    -> AssocListF ts xs (Compose Opt f)
    -> ([OptError], [Optparse.Mod Optparse.CommandFields (VariantF xs f)])

instance ExplSubcommands Z ts xs '[] => Subcommands ts xs where
  mapSubcommand = explMapSubcommand @Z @ts @xs @'[] SZ


-- | More general version of 'Subcommands'.
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
    -> s
    -> AssocListF ts xs (Compose Opt f)
    -> ([OptError], [Optparse.Mod Optparse.CommandFields (VariantF (acc ++ xs) f)])

instance ExplSubcommands n '[] '[] acc where
  explMapSubcommand _ _ _ = ([], [])

-- ok wait
-- hear me out:
instance
    ( ExplSubcommands (S n) ts xs (as ++ '[x])
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
        (errs, sc)
          = subcommand
        (errs', rest)
          = hgcastWith (proof @as @x @xs)
          $ explMapSubcommand
              @(S n) @ts @xs @(as ++ '[x])
              (SS n) srcs opts

      in (errs ++ errs', sc : rest)

    where
      subcommand
        = let
            -- TODO: accumulate errors
            (errs, src)
              = accumSourceResults $ runSource srcs opt
            parser
              = mkOptparseParser src opt
            tag
              = symbolVal (Proxy :: Proxy t)
            cmd
              = Optparse.command tag
              $ injectPosF n
              <$> Optparse.info (Optparse.helper <*> parser) mempty
          in (errs, cmd)
