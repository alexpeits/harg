{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module Options.Harg.Sources.Optparse where

import           Data.Functor.Compose       (Compose (..))
import           Data.Kind                  (Type)
import           GHC.Generics               (Generic)

import qualified Data.Barbie                as B
import qualified Options.Applicative        as Optparse

import qualified Options.Harg.Cmdline       as Cli
import           Options.Harg.Sources.Types
import           Options.Harg.Types


data OptparseSource (f :: Type -> Type) = OptparseSource
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

newtype OptparseSourceVal = OptparseSourceVal Args

instance GetSource OptparseSource f where
  type SourceVal OptparseSource = OptparseSourceVal
  getSource HargCtx{..} _
    = pure (OptparseSourceVal _hcArgs)

instance
    ( B.FunctorB a
    , B.TraversableB a
    ) => RunSource OptparseSourceVal a where
  runSource (OptparseSourceVal args) opt
    = [runOptparseSource args opt]

runOptparseSource
  :: forall a f.
     ( B.FunctorB a
     , B.TraversableB a
     , Applicative f
     )
  => Args
  -> a (Compose Opt f)
  -> Either SourceRunError (a (Compose SourceRunResult f))
runOptparseSource args opts
  = go $ B.bsequence $ B.bmap mkParser opts
  where
    go
      :: Optparse.Parser (a (Compose Maybe f))
      -> Either SourceRunError (a (Compose SourceRunResult f))
    go parser
      = case Cli.execParserPure args parser of
          -- TODO
          Optparse.Failure err
            -> Left $ SourceRunError Nothing "Optparse" (show err)
          Optparse.Success res
            -> Right $ B.bmap toSuccess res
          _
            -> Left $ SourceRunError Nothing "Optparse" "unknown"

    toSuccess
      :: Compose Maybe f x
      -> Compose SourceRunResult f x
    toSuccess
      = Compose
      . maybe OptNotFound OptParsed
      . getCompose

    mkParser
      :: forall x.
         Compose Opt f x
      -> Compose Optparse.Parser (Compose Maybe f) x
    mkParser
      -- TODO
      = Cli.mkParser (Compose Nothing)
