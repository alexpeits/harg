{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Sources.YAML where

import           Control.Exception          (displayException)
import qualified Data.ByteString            as BS
import           Data.Functor.Compose       (Compose (..))
import           Data.Functor.Identity      (Identity(..))
import           GHC.Generics               (Generic)

import qualified Data.Barbie                as B
import qualified Data.Yaml                  as YAML

import           Options.Harg.Sources.Types
import           Options.Harg.Types
import           Options.Harg.Util


newtype YAMLSource f = YAMLSource (f FilePath)
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

newtype YAMLSourceVal = YAMLSourceVal BS.ByteString

instance GetSource YAMLSource Identity where
  type SourceVal YAMLSource = YAMLSourceVal
  getSource (YAMLSource (Identity path))
    = YAMLSourceVal <$> readFileBS path

instance {-# OVERLAPS #-}
    ( YAML.FromJSON (a Maybe)
    , B.FunctorB a
    ) => RunSource YAMLSourceVal a where
  runSource (YAMLSourceVal j) opt
    = [runYAMLSource j opt]

runYAMLSource
  :: forall a f.
     ( B.FunctorB a
     , YAML.FromJSON (a Maybe)
     , Applicative f
     )
  => BS.ByteString
  -> a (Compose Opt f)
  -> a (Compose SourceRunResult f)
runYAMLSource yaml opt
  = let
      res :: Either YAML.ParseException (a Maybe)
      res
        = YAML.decodeEither' yaml
      toSuccess :: Maybe x -> Compose SourceRunResult f x
      toSuccess mx
        = Compose $ pure <$> maybe OptNotFound OptParsed mx
      toFailure
        :: YAML.ParseException
        -> Compose Opt f x
        -> Compose SourceRunResult f x
      toFailure exc (Compose o)
        = Compose $ OptFoundNoParse (toOptError o (displayException exc))
    in case res of
         Right v  -> B.bmap toSuccess v
         Left exc -> B.bmap (toFailure exc) opt
