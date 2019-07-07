{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Sources where

import           Data.Foldable             (foldr')
import           Data.Kind                 (Type)
import           System.Environment        (getEnvironment)
import           GHC.Generics              (Generic)
import           Data.Functor.Identity     (Identity(..))

import qualified Data.Aeson                as JSON
import qualified Data.Barbie               as B
import qualified Options.Applicative       as Optparse

import           Options.Harg.Sources.Env
import           Options.Harg.Sources.JSON
import           Options.Harg.Types
import           Options.Harg.Het.Prod
import           Options.Harg.Cmdline


accumSourceResults
  :: forall a. B.TraversableB a
  => [a SourceParseResult]
  -> ([OptError], [a Maybe])
accumSourceResults
  = foldr' accumResult ([], [])
  where
    accumResult res (e, a)
      = case B.btraverse go res of
          (e', a') -> (e' <> e, a' : a)
    go
      = \case
          OptFoundNoParse e -> ([e], Nothing)
          OptParsed a       -> ([], Just a)
          _                 -> ([], Nothing)

data Env (f :: Type -> Type) = Env
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

newtype Jason f = Jason (f String)
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

data EnvSource = EnvSource Environment
data JSONSource = JSONSource JSON.Value

type family SourceVal (s :: (Type -> Type) -> Type) :: Type where
  SourceVal Env   = EnvSource
  SourceVal Jason = JSONSource

class RunSource (s :: (Type -> Type) -> Type) a where
  runSource' :: SourceVal s -> a Opt -> a SourceParseResult

instance B.FunctorB a => RunSource Env a where
  runSource' (EnvSource e) = runEnvVarSource e

instance ( B.FunctorB a
         , JSON.FromJSON (a Maybe)
         ) => RunSource Jason a where
  runSource' (JSONSource j) = runJSONSource j

class ( B.TraversableB a
      , B.ProductB a
      , B.FunctorB a
      ) => GetSources c a where
  getSources' :: c -> a Opt -> IO ([OptError], [a Maybe])

instance ( B.TraversableB a
         , B.ProductB a
         , B.FunctorB a
         ) => GetSources (Env f) a where
  getSources' _ opts
    = do
        env <- getEnvironment
        let
          (errs, res)
            = accumSourceResults [runSource' @Env env opts]
        pure (errs, res)

dummyOpt :: Opt String
dummyOpt
  = Opt
      { _optLong = Nothing
      , _optShort = Nothing
      , _optHelp = Nothing
      , _optMetavar = Nothing
      , _optEnvVar = Nothing
      , _optDefault = Just ""
      , _optReader = pure
      , _optType = FlagOptType ""
      }

instance ( B.TraversableB a
         , B.ProductB a
         , B.FunctorB a
         , JSON.FromJSON (a Maybe)
         ) => GetSources (Jason Identity) a where
  getSources' (Jason (Identity s)) opts
    = do
        Just json <- getJSON s
        let
          (errs, res)
            = accumSourceResults [runSource' @Jason json opts]
        pure (errs, res)

jsonOpt :: String -> Opt String
jsonOpt s
  = Opt
      { _optLong = Just s
      , _optShort = Nothing
      , _optHelp = Just "JSON config path"
      , _optMetavar = Nothing
      , _optEnvVar = Nothing
      , _optDefault = Nothing
      , _optReader = pure
      , _optType = OptionOptType
      }

instance ( GetSources (l Identity) a
         , GetSources (r Identity) a
         , B.TraversableB a
         , B.ProductB a
         , B.FunctorB a
         -- , JSON.FromJSON (a Maybe)
         ) => GetSources ((l :* r) Identity) a where
  getSources' (l :* r) f
    = do
        (le, lv) <- getSources' l f
        (re, rv) <- getSources' r f
        pure (le ++ re, lv ++ rv)

-- type family ConstraintsFor (a :: Type) :: Type -> Constraint

-- type instance ConstraintsFor EnvS = EmptyConstraint
-- type instance ConstraintsFor JSONS = JSON.FromJSON

-- type family GatherConstraints (srcs :: [Type]) (a :: Type) :: Constraint where
--   GatherConstraints '[] _ = ()
--   GatherConstraints (s ': ss) a = (ConstraintsFor s a, GatherConstraints ss a)

-- class EmptyConstraint (a :: Type)
-- instance EmptyConstraint (a :: Type)
