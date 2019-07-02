{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Parser where

import           Data.Functor.Identity      (Identity (..))
import           Data.Kind                  (Type)
import           Data.Proxy                 (Proxy (..))
import           GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)

import qualified Data.Barbie                as B
import qualified Options.Applicative        as Optparse

import           Options.Harg.Cmdline
import           Options.Harg.Het.AssocList
import           Options.Harg.Het.Nat
import           Options.Harg.Het.Proofs
import           Options.Harg.Het.Variant
import           Options.Harg.Types


getOptParser
  :: B.TraversableB a
  => [ParserSource]
  -> a Opt
  -> (Parser (a Identity))
getOptParser sources opts
  = B.btraverse (fmap Identity <$> toParser sources) opts

getOptParserSubcommand
  :: forall xs ts.
     ( B.TraversableB (VariantF xs)
     , Subcommands Z ts xs '[]
     )
  => [ParserSource]
  -> AssocListF ts xs Opt
  -> Parser (VariantF xs Identity)
getOptParserSubcommand sources alist
  = let
      (commands, err)
        = mapSubcommand @Z @ts @xs @'[] SZ sources alist
      parser
        = Optparse.subparser (mconcat commands)
    in Parser parser err

-- subcommands
class Subcommands
    (n :: Nat)
    (ts :: [Symbol])
    (xs :: [(Type -> Type) -> Type])
    (acc :: [(Type -> Type) -> Type]) where
  mapSubcommand
    :: SNat n
    -> [ParserSource]
    -> AssocListF ts xs Opt
    -> ([Optparse.Mod Optparse.CommandFields (VariantF (acc ++ xs) Identity)], [OptError])

instance Subcommands n '[] '[] acc where
  mapSubcommand _ _ _ = ([], [])

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

  mapSubcommand n sources (ACons opt opts)
    = let
        (sc, err) = subcommand
        (rest, errs) = hgcastWith (proof @as @x @xs)
                         (mapSubcommand @(S n) @ts @xs @(as ++ '[x]) (SS n) sources opts)
      in (sc : rest, err <> errs)

    where

      subcommand
        :: ( Optparse.Mod Optparse.CommandFields (VariantF (as ++ (x ': xs)) Identity)
           , [OptError]
           )
      subcommand
        = let
            (Parser parser err)
              = B.btraverse (fmap Identity <$> toParser sources) opt
            cmd
              = Optparse.command tag
              $ injectPosF n
              <$> Optparse.info (Optparse.helper <*> parser) mempty
          in (cmd, err)

      tag
        = symbolVal (Proxy :: Proxy t)

-- The following allows to use one function for commands + subcommands
type family OptResult' a where
  OptResult' (AssocListF ts xs) = VariantF xs Identity
  OptResult' a = a Identity

class GetParser a where
  type OptResult a :: Type
  getParser :: [ParserSource] -> a -> Parser (OptResult a)

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
