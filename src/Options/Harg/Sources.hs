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


data EnvSource (f :: Type -> Type) = EnvSource
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

newtype JSONSource f = JSONSource (f String)
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)


newtype EnvSourceVal = EnvSourceVal Environment
newtype JSONSourceVal = JSONSourceVal JSON.Value


type family SourceVal (s :: (Type -> Type) -> Type) :: [Type] where
  SourceVal EnvSource  = '[EnvSourceVal]
  SourceVal JSONSource = '[JSONSourceVal]
  SourceVal (a :* b)   = SourceVal a ++ SourceVal b


class RunSource (s :: [Type]) a where
  runSource :: Applicative f => HList s -> a (Compose Opt f) -> [a (Compose SourceParseResult f)]

instance {-# OVERLAPS #-} B.FunctorB a => RunSource '[EnvSourceVal] a where
  runSource (HCons (EnvSourceVal e) HNil) opt = [runEnvVarSource e opt]

instance {-# OVERLAPS #-}
         ( JSON.FromJSON (a Maybe)
         , B.FunctorB a
         ) => RunSource '[JSONSourceVal] a where
  runSource (HCons (JSONSourceVal j) HNil) opt = [runJSONSource j opt]

instance ( RunSource xs a
         , RunSource '[x] a
         ) => RunSource (x ': xs) a where
  runSource (HCons x xs) opt = runSource (HCons x HNil) opt ++ runSource xs opt

instance RunSource '[] a where
  runSource HNil _ = []


class GetSource c f where
  getSource :: c f -> IO (HList (SourceVal c))

instance GetSource EnvSource f where
  getSource _
    = do
        env <- getEnvironment
        pure $ HCons (EnvSourceVal env) HNil

instance GetSource JSONSource Identity where
  getSource (JSONSource (Identity s))
    = do
        Just json <- getJSON s
        pure $ HCons (JSONSourceVal json) HNil

instance ( GetSource l f
         , GetSource r f
         ) => GetSource (l :* r) f where
  getSource (l :* r)
    = (+++) <$> getSource l <*> getSource r

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
