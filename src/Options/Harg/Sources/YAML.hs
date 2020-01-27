{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Sources.YAML
  ( YAMLSource (..)
  ) where

import           Control.Exception          (displayException)
import qualified Data.ByteString            as BS
import           Data.Functor.Compose       (Compose (..))
import           Data.Functor.Identity      (Identity(..))
import           GHC.Generics               (Generic)

import qualified Data.Barbie                as B
import qualified Data.Yaml                  as YAML

import           Options.Harg.Sources.Types
import           Options.Harg.Types
import           Options.Harg.Util          (readFileBS)


-- | Source that enables a parser to read options from a YAML file.
newtype YAMLSource f = YAMLSource (f ConfigFile)
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

-- | The result of reading a YAML file. @YAMLSourceNotRequired@ is used when
-- the user has specified @defaultVal NoConfigFile@. It holds the contents of
-- the YAML file as a 'BS.ByteString'.
data YAMLSourceVal
  = YAMLSourceVal BS.ByteString
  | YAMLSourceNotRequired

instance GetSource YAMLSource Identity where
  type SourceVal YAMLSource = YAMLSourceVal
  getSource _ctx (YAMLSource (Identity (ConfigFile path)))
    = YAMLSourceVal <$> readFileBS path
  getSource _ctx (YAMLSource (Identity NoConfigFile))
    = pure YAMLSourceNotRequired

instance
    ( YAML.FromJSON (a Maybe)
    , B.FunctorB a
    ) => RunSource YAMLSourceVal a where
  runSource (YAMLSourceVal j) opt
    = [runYAMLSource j opt]
  runSource YAMLSourceNotRequired _
    = []

runYAMLSource
  :: forall a f.
     ( B.FunctorB a
     , YAML.FromJSON (a Maybe)
     , Applicative f
     )
  => BS.ByteString
  -> a (Compose Opt f)
  -> Either SourceRunError (a (Compose SourceRunResult f))
runYAMLSource yaml _opt
  = case res of
      Right v  -> Right $ B.bmap toSuccess v
      Left exc -> Left $ toError exc
  where
    res :: Either YAML.ParseException (a Maybe)
    res
      = YAML.decodeEither' yaml

    toSuccess :: Maybe x -> Compose SourceRunResult f x
    toSuccess mx
      = Compose $ pure <$> maybe OptNotFound OptParsed mx

    toError :: YAML.ParseException -> SourceRunError
    toError exc
      = SourceRunError Nothing "YAMLSource" (displayException exc)
