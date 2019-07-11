{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Sources.JSON where

import           Data.Functor.Compose       (Compose (..))
import           Data.Functor.Identity      (Identity(..))
import           GHC.Generics               (Generic)

import qualified Data.Aeson                 as JSON
import qualified Data.Barbie                as B

import           Options.Harg.Sources.Types
import           Options.Harg.Types
import           Options.Harg.Util


newtype JSONSource f = JSONSource (f FilePath)
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

newtype JSONSourceVal = JSONSourceVal JSON.Value

instance GetSource JSONSource Identity where
  type SourceVal JSONSource = JSONSourceVal
  getSource _ctx (JSONSource (Identity path))
    = do
        contents <- readFileLBS path
        case JSON.eitherDecode contents of
          Right json
            -> pure $ JSONSourceVal json
          Left err
            -> printErrAndExit
               $ "Error decoding " <> path <> " to JSON: " <> err

instance
    ( JSON.FromJSON (a Maybe)
    , B.FunctorB a
    ) => RunSource JSONSourceVal a where
  runSource (JSONSourceVal j) opt
    = [runJSONSource j opt]

runJSONSource
  :: forall a f.
     ( B.FunctorB a
     , JSON.FromJSON (a Maybe)
     , Applicative f
     )
  => JSON.Value
  -> a (Compose Opt f)
  -> a (Compose SourceRunResult f)
runJSONSource json opt
  = let
      res :: JSON.Result (a Maybe)
      res
        = JSON.fromJSON json
      toSuccess :: Maybe x -> Compose SourceRunResult f x
      toSuccess mx
        = Compose $ pure <$> maybe OptNotFound OptParsed mx
      toFailure :: Compose Opt f x -> Compose SourceRunResult f x
      toFailure _
        = Compose $ pure <$> OptNotFound
    in case res of
         JSON.Success v -> B.bmap toSuccess v
         JSON.Error _e  -> B.bmap toFailure opt
