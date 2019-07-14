{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Sources.Types where

import Data.Functor.Compose  (Compose (..))
import Data.Kind             (Type)
import           Data.String (IsString(..))

import Options.Harg.Het.Prod
import Options.Harg.Types


data SourceRunResult a
  = OptNotFound
  | OptFoundNoParse OptError
  | OptParsed a
  deriving Functor

class GetSource
    (c :: (Type -> Type) -> Type)
    (f :: (Type -> Type)) where
  type SourceVal c :: Type
  getSource :: HargCtx -> c f -> IO (SourceVal c)

instance
    ( GetSource l f
    , GetSource r f
    ) => GetSource (l :* r) f where
  type SourceVal (l :* r) = (SourceVal l, SourceVal r)
  getSource ctx (l :* r)
    = (,) <$> getSource ctx l <*> getSource ctx r

data ConfigFile
  = ConfigFile FilePath
  | NoConfigFile

instance IsString ConfigFile where
  fromString = ConfigFile

class RunSource s a where
  runSource
    :: Applicative f
    => s
    -> a (Compose Opt f)
    -> [a (Compose SourceRunResult f)]

instance
    ( RunSource l a
    , RunSource r a
    ) => RunSource (l, r) a where
  runSource (l, r) opt
    = runSource l opt ++ runSource r opt

instance RunSource () a where
  runSource () _
    = []
