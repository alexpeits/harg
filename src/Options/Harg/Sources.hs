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
import           Options.Harg.Het.Proofs
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

data HList (as :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

type family SourceVal (s :: (Type -> Type) -> Type) :: [Type] where
  SourceVal Env   = '[EnvSource]
  SourceVal Jason = '[JSONSource]
  SourceVal (a :* b) = SourceVal a ++ SourceVal b

class RunSource (s :: [Type]) a where
  runSource' :: HList s -> a Opt -> [a SourceParseResult]

-- class RunSource (s :: (Type -> Type) -> Type) a where
--   runSource' :: HList (SourceVal s) -> a Opt -> a SourceParseResult

instance {-# OVERLAPS #-} B.FunctorB a => RunSource '[EnvSource] a where
  runSource' (HCons (EnvSource e) HNil) opt = [runEnvVarSource e opt]

-- instance B.FunctorB a => RunSource Env a where
--   runSource' (HCons (EnvSource e) HNil) = runEnvVarSource e

-- instance ( B.FunctorB a
--          , JSON.FromJSON (a Maybe)
--          ) => RunSource Jason a where
--   runSource' (HCons (JSONSource j) HNil) = runJSONSource j

instance {-# OVERLAPS #-}
         ( JSON.FromJSON (a Maybe)
         , B.FunctorB a
         ) => RunSource '[JSONSource] a where
  runSource' (HCons (JSONSource j) HNil) opt = [runJSONSource j opt]

instance ( RunSource xs a
         , RunSource '[x] a
         ) => RunSource (x ': xs) a where
  runSource' (HCons x xs) opt = runSource' (HCons x HNil) opt ++ runSource' xs opt

instance RunSource '[] a where
  runSource' HNil _ = []

class GetSources c f (a :: (Type -> Type) -> Type) where
  getSources' :: c f -> IO (HList (SourceVal c)) -- IO ([OptError], [a Maybe])

instance GetSources Env f a where
  getSources' _
    = do
        env <- getEnvironment
        pure $ HCons (EnvSource env) HNil
        -- let
        --   (errs, res)
        --     = accumSourceResults [runSource' @Env env opts]
        -- pure (errs, res)

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

instance (JSON.FromJSON (a Maybe)
         ) => GetSources Jason Identity a where
  getSources' (Jason (Identity s))
    = do
        Just json <- getJSON s
        pure $ HCons (JSONSource json) HNil
        -- let
        --   (errs, res)
        --     = accumSourceResults [runSource' @Jason json opts]
        -- pure (errs, res)

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

instance ( GetSources l f a
         , GetSources r f a
         ) => GetSources (l :* r) f a where
  getSources' (l :* r)
    = do
        ls <- getSources' @_ @_ @a l
        rs <- getSources' @_ @_ @a r
        pure (ls +++ rs) -- (le ++ re, lv ++ rv)

(+++) :: HList as -> HList bs -> HList (as ++ bs)
HNil +++ ys = ys
(HCons x xs) +++ ys = HCons x (xs +++ ys)

-- type family ConstraintsFor (a :: Type) :: Type -> Constraint

-- type instance ConstraintsFor EnvS = EmptyConstraint
-- type instance ConstraintsFor JSONS = JSON.FromJSON

-- type family GatherConstraints (srcs :: [Type]) (a :: Type) :: Constraint where
--   GatherConstraints '[] _ = ()
--   GatherConstraints (s ': ss) a = (ConstraintsFor s a, GatherConstraints ss a)

-- class EmptyConstraint (a :: Type)
-- instance EmptyConstraint (a :: Type)
