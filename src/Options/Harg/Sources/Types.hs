{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Sources.Types where

import Data.Functor.Compose    (Compose (..))
import Data.Kind               (Type)

import Options.Harg.Het.Proofs
import Options.Harg.Het.HList
import Options.Harg.Het.Prod
import Options.Harg.Types

data SourceParseResult a
  = SourceNotAvailable
  | OptNotFound
  | OptFoundNoParse OptError
  | OptParsed a
  deriving Functor

data family SourceValFor (s :: (Type -> Type) -> Type) :: Type

type family SourceVal (s :: (Type -> Type) -> Type) :: [Type] where
  SourceVal (a :* b) = SourceVal a ++ SourceVal b
  SourceVal src      = '[SourceValFor src]

class RunSource (s :: [Type]) a where
  runSource
    :: Applicative f
    => HList s
    -> a (Compose Opt f)
    -> [a (Compose SourceParseResult f)]

instance
    ( RunSource xs a
    , RunSource '[x] a
    ) => RunSource (x ': xs) a where
  runSource (HCons x xs) opt
    = runSource (HCons x HNil) opt ++ runSource xs opt

instance RunSource '[] a where
  runSource HNil _
    = []

class GetSource c f where
  getSource :: c f -> IO (HList (SourceVal c))

instance
    ( GetSource l f
    , GetSource r f
    ) => GetSource (l :* r) f where
  getSource (l :* r)
    = (+++) <$> getSource l <*> getSource r
