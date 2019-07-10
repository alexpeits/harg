{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Options.Harg.Sources.Dhall where

import           Control.Exception          (displayException, catch, IOException)
import           Data.Functor.Compose       (Compose (..))
import           Data.Functor.Identity      (Identity(..))
import           GHC.Generics               (Generic)

import qualified Data.Barbie                as B
import qualified Dhall
import           Dhall.Core                 (Expr)
import           Dhall.Parser               (Src)
import           Dhall.TypeCheck            (X)

import           Options.Harg.Sources.Types
import           Options.Harg.Types
import           Options.Harg.Util


newtype DhallSource f = DhallSource (f String)
  deriving (Generic)
  deriving anyclass (B.FunctorB, B.TraversableB, B.ProductB)

newtype DhallSourceVal = DhallSourceVal (Expr Src X)

instance GetSource DhallSource Identity where
  type SourceVal DhallSource = DhallSourceVal
  getSource (DhallSource (Identity path))
    = DhallSourceVal <$> readDhall path

readDhall
  :: FilePath
  -> IO (Expr Src X)
readDhall path
  = do
      contents <- readFileTx path
      Dhall.inputExpr contents `catch` handleError
  where
    handleError
      :: IOException
      -> IO (Expr Src X)
    handleError err
      = printErrAndExit
      $ "Error decoding " <> path <> " to Dhall: "
        <> displayException err

instance {-# OVERLAPS #-}
    ( Dhall.Interpret (a Id)
    , B.FunctorB a
    ) => RunSource DhallSourceVal a where
  runSource (DhallSourceVal j) opt
    = [runDhallSource j opt]

newtype Id a
  = Id { getId :: a }
  deriving newtype Dhall.Interpret

runDhallSource
  :: forall a f.
     ( B.FunctorB a
     , Dhall.Interpret (a Id)
     , Applicative f
     )
  => Expr Src X
  -> a (Compose Opt f)
  -> a (Compose SourceParseResult f)
runDhallSource expr opt
  = let
      res :: Maybe (a Id)
      res
        = Dhall.rawInput Dhall.auto expr
      toSuccess :: Id x -> Compose SourceParseResult f x
      toSuccess (Id x)
        = Compose $ pure <$> maybe OptNotFound OptParsed (Just x)
      toFailure :: Compose Opt f x -> Compose SourceParseResult f x
      toFailure _
        = Compose $ pure <$> OptNotFound
    in case res of
         Just v  -> B.bmap toSuccess v
         Nothing -> B.bmap toFailure opt
