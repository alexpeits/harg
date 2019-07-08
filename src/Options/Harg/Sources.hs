{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Options.Harg.Sources where

import           Data.Foldable             (foldr')
import           Data.Functor.Compose      (Compose (..))
import           Data.Functor.Identity     (Identity(..))
import           Data.Kind                 (Type)
import           GHC.Generics              (Generic)
import           System.Environment        (getEnvironment)

import qualified Data.Aeson                as JSON
import qualified Data.Barbie               as B

import           Options.Harg.Het.HList
import           Options.Harg.Het.Prod
import           Options.Harg.Het.Proofs
import           Options.Harg.Sources.Env
import           Options.Harg.Sources.JSON
import           Options.Harg.Types


accumSourceResults
  :: forall a f.
     ( B.TraversableB a
     )
  => [a (Compose SourceParseResult f)]
  -> ([OptError], [a (Compose Maybe f)])
accumSourceResults
  = foldr' accumResult ([], [])
  where
    accumResult
      :: a (Compose SourceParseResult f)
      -> ([OptError], [a (Compose Maybe f)])
      -> ([OptError], [a (Compose Maybe f)])
    accumResult res (e, a)
      = case B.btraverse go res of
          (e', a') -> (e' <> e, a' : a)
    go
      :: Compose SourceParseResult f x
      -> ([OptError], Compose Maybe f x)
    go x
      = case getCompose x of
          OptFoundNoParse e -> ([e], Compose Nothing)
          OptParsed a       -> ([], Compose (Just a))
          _                 -> ([], Compose Nothing)

data Env (f :: Type -> Type) = Env
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

instance Show (Env Identity) where show _ = "ENV"

newtype Jason f = Jason (f String)
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

instance Show (Jason Identity) where show (Jason (Identity s)) = "JSON: " <> s

newtype EnvSource = EnvSource Environment
  deriving Show
newtype JSONSource = JSONSource JSON.Value
  deriving Show

type family SourceVal (s :: (Type -> Type) -> Type) :: [Type] where
  SourceVal Env   = '[EnvSource]
  SourceVal Jason = '[JSONSource]
  SourceVal (a :* b) = SourceVal a ++ SourceVal b

class RunSource (s :: [Type]) a where
  runSource :: Applicative f => HList s -> a (Compose Opt f) -> [a (Compose SourceParseResult f)]

instance {-# OVERLAPS #-} B.FunctorB a => RunSource '[EnvSource] a where
  runSource (HCons (EnvSource e) HNil) opt = [runEnvVarSource e opt]

instance {-# OVERLAPS #-}
         ( JSON.FromJSON (a Maybe)
         , B.FunctorB a
         ) => RunSource '[JSONSource] a where
  runSource (HCons (JSONSource j) HNil) opt = [runJSONSource j opt]

instance ( RunSource xs a
         , RunSource '[x] a
         ) => RunSource (x ': xs) a where
  runSource (HCons x xs) opt = runSource (HCons x HNil) opt ++ runSource xs opt

instance RunSource '[] a where
  runSource HNil _ = []

class GetSource c f where
  getSource :: c f -> IO (HList (SourceVal c))

instance GetSource Env f where
  getSource _
    = do
        env <- getEnvironment
        pure $ HCons (EnvSource env) HNil

instance GetSource Jason Identity where
  getSource (Jason (Identity s))
    = do
        Just json <- getJSON s
        pure $ HCons (JSONSource json) HNil

instance ( GetSource l f
         , GetSource r f
         ) => GetSource (l :* r) f where
  getSource (l :* r)
    = do
        ls <- getSource l
        rs <- getSource r
        pure (ls +++ rs) -- (le ++ re, lv ++ rv)

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
