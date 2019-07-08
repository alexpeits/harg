{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module Options.Harg.Sources.Env where

import           Data.Functor.Compose       (Compose (..))
import           Data.Kind                  (Type)
import           Data.List                  (find)
import           GHC.Generics               (Generic)
import           System.Environment         (getEnvironment)

import qualified Data.Barbie                as B

import           Options.Harg.Het.HList
import           Options.Harg.Sources.Types
import           Options.Harg.Types


type Environment
  = [(String, String)]

data EnvSource (f :: Type -> Type) = EnvSource
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

newtype EnvSourceVal = EnvSourceVal Environment

instance {-# OVERLAPS #-}
    B.FunctorB a => RunSource '[EnvSourceVal] a where
  runSource (HCons (EnvSourceVal e) HNil) opt
    = [runEnvVarSource e opt]

instance GetSource EnvSource f where
  type SourceVal EnvSource = '[EnvSourceVal]
  getSource _
    = do
        env <- getEnvironment
        pure $ HCons (EnvSourceVal env) HNil

lookupEnv
  :: Environment
  -> String
  -> Maybe String
lookupEnv env x
  = snd <$> find ((== x) . fst) env

runEnvVarSource
  :: forall a f.
     ( B.FunctorB a
     , Applicative f
     )
  => Environment
  -> a (Compose Opt f)
  -> a (Compose SourceParseResult f)
runEnvVarSource env
  = B.bmap go
  where
    go :: Compose Opt f x -> Compose SourceParseResult f x
    go (Compose opt@Opt{..})
      = case _optEnvVar of
          Nothing
            -> Compose $ pure <$> OptNotFound
          Just envVar
            -> Compose $ maybe OptNotFound tryParse (lookupEnv env envVar)
      where
        tryParse
          = either
              (OptFoundNoParse . toOptError opt)
              OptParsed
          . _optReader
