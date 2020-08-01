{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Options.Harg.Sources.Types
  ( GetSource (..),
    RunSource (..),
    ConfigFile (..),
    SourceRunResult (..),
    SourceRunError (..),
    sourceRunError,
  )
where

import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import Options.Harg.Het.Prod ((:*) (..))
import Options.Harg.Types

-- | Holds errors that occur when running a source.
data SourceRunResult a
  = -- | Source doesn't include the option
    OptNotFound
  | -- | Successful parsing
    OptParsed a
  deriving (Functor)

data SourceRunError = SourceRunError
  { _sreOpt :: Maybe SomeOpt,
    _sreSourceName :: String,
    _sreError :: String
  }

-- | Create a 'SourceRunError' by existentially wrapping an option in 'SomeOpt'.
sourceRunError ::
  forall a.
  Opt a ->
  String ->
  String ->
  SourceRunError
sourceRunError =
  SourceRunError . Just . SomeOpt

-- | This class enables a type that describes a source to fetch the source
-- contents, potentially producing side effects (e.g. reading a file).
class
  GetSource
    (c :: (Type -> Type) -> Type)
    (f :: (Type -> Type))
  where
  -- | The type that will be returned when the source is read.
  type SourceVal c :: Type

  getSource :: HargCtx -> c f -> IO (SourceVal c)

instance
  ( GetSource l f,
    GetSource r f
  ) =>
  GetSource (l :* r) f
  where
  type SourceVal (l :* r) = (SourceVal l, SourceVal r)
  getSource ctx (l :* r) =
    (,) <$> getSource ctx l <*> getSource ctx r

-- | This class is used to run the result of running 'getSource' on the
-- configuration options. In order for it to work, all types used in the
-- source configuration need to have a 'GetSource' instance, and their
-- associated 'SourceVal' types need to have a 'RunSource' instance.
class RunSource s a where
  runSource ::
    Applicative f =>
    s ->
    a (Compose Opt f) ->
    [Either SourceRunError (a (Compose SourceRunResult f))]

instance
  ( RunSource l a,
    RunSource r a
  ) =>
  RunSource (l, r) a
  where
  runSource (l, r) opt =
    runSource l opt ++ runSource r opt

instance RunSource () a where
  runSource () _ =
    []

-- | This type describes configuration files, for use with e.g. the JSON
-- source. The reason to not use 'FilePath' directly is that the user might
-- prefer to do nothing if the option for the config file has not been not
-- provided, and there's no default. Because this type has an 'IsString'
-- instance, it's very easy to define an option. For example, to define a json
-- source with a default value:
--
-- @
--   srcOpt :: JSONSource Opt
--   srcOpt = JSONSource jsonOpt
--     where
--       jsonOpt
--         = optionWith strParser
--             ( long "json-config"
--             . defaultVal (ConfigFile "~/config.json")
--             )
-- @
--
-- And an optional JSON source:
--
-- @
--   srcOpt :: JSONSource Opt
--   srcOpt = JSONSource jsonOpt
--     where
--       jsonOpt
--         = optionWith strParser
--             ( long "json-config"
--             . defaultVal NoConfigFile
--             )
-- @
data ConfigFile
  = ConfigFile FilePath
  | NoConfigFile

instance IsString ConfigFile where
  fromString = ConfigFile
