{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Sources.JSON
  ( JSONSource (..)
  ) where

import qualified Data.ByteString.Lazy       as LBS
import           Data.Functor.Compose       (Compose (..))
import           Data.Functor.Identity      (Identity(..))
import           GHC.Generics               (Generic)

import qualified Data.Aeson                 as JSON
import qualified Data.Barbie                as B

import           Options.Harg.Sources.Types
import           Options.Harg.Types
import           Options.Harg.Util          (readFileLBS)

-- | Source that enables a parser to read options from a JSON file.
newtype JSONSource f = JSONSource (f ConfigFile)
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

-- | The result of reading a JSON file. @JSONSourceNotRequired@ is used when
-- the user has specified @defaultVal NoConfigFile@. It holds the contents of
-- the JSON file as a 'JSON.Value'.
data JSONSourceVal
  = JSONSourceVal LBS.ByteString
  | JSONSourceNotRequired

instance GetSource JSONSource Identity where
  type SourceVal JSONSource = JSONSourceVal
  getSource _ctx (JSONSource (Identity (ConfigFile path)))
    = JSONSourceVal <$> readFileLBS path
  getSource _ctx (JSONSource (Identity NoConfigFile))
    = pure JSONSourceNotRequired

instance
    ( JSON.FromJSON (a Maybe)
    , B.FunctorB a
    ) => RunSource JSONSourceVal a where
  runSource (JSONSourceVal j) opt
    = [runJSONSource j opt]
  runSource JSONSourceNotRequired _
    = []

runJSONSource
  :: forall a f.
     ( B.FunctorB a
     , JSON.FromJSON (a Maybe)
     , Applicative f
     )
  => LBS.ByteString
  -> a (Compose Opt f)
  -> Either SourceRunError (a (Compose SourceRunResult f))
runJSONSource json _opt
  = case res of
      Right v  -> Right $ B.bmap toSuccess v
      Left exc -> Left $ toError exc
  where
    res :: Either String (a Maybe)
    res
      = JSON.eitherDecode json

    toSuccess :: Maybe x -> Compose SourceRunResult f x
    toSuccess mx
      = Compose $ pure <$> maybe OptNotFound OptParsed mx

    toError :: String -> SourceRunError
    toError
      = SourceRunError Nothing "JSONSource"
