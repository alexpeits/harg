{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module Options.Harg.Sources.JSON where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Functor.Compose       (Compose (..))
import           Data.Functor.Identity      (Identity(..))
import           GHC.Generics               (Generic)

import qualified Data.Aeson                 as JSON
import qualified Data.Barbie                as B

import           Options.Harg.Types
import           Options.Harg.Het.HList
import           Options.Harg.Sources.Types


newtype JSONSource f = JSONSource (f String)
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

newtype JSONSourceVal = JSONSourceVal JSON.Value

instance {-# OVERLAPS #-}
    ( JSON.FromJSON (a Maybe)
    , B.FunctorB a
    ) => RunSource '[JSONSourceVal] a where
  runSource (HCons (JSONSourceVal j) HNil) opt
    = [runJSONSource j opt]

instance GetSource JSONSource Identity where
  type SourceVal JSONSource = '[JSONSourceVal]
  getSource (JSONSource (Identity s))
    = do
        Just json <- getJSON s
        pure $ HCons (JSONSourceVal json) HNil

getJSON
  :: String
  -> IO (Maybe JSON.Value)
getJSON path
  = do
      contents <- LBS.readFile path
      pure $ JSON.decode contents

runJSONSource
  :: forall a f.
     ( B.FunctorB a
     , JSON.FromJSON (a Maybe)
     , Applicative f
     )
  => JSON.Value
  -> a (Compose Opt f)
  -> a (Compose SourceParseResult f)
runJSONSource json opt
  = let
      res :: JSON.Result (a Maybe)
      res
        = JSON.fromJSON json
      toSuccess :: Maybe x -> Compose SourceParseResult f x
      toSuccess mx
        = Compose $ pure <$> maybe OptNotFound OptParsed mx
      toFailure :: Compose Opt f x -> Compose SourceParseResult f x
      toFailure _
        = Compose $ pure <$> OptNotFound
    in case res of
         JSON.Success v -> B.bmap toSuccess v
         JSON.Error _e  -> B.bmap toFailure opt
