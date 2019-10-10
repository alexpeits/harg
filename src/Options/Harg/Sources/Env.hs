{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module Options.Harg.Sources.Env where

import           Data.Functor.Compose       (Compose (..))
import           Data.Kind                  (Type)
import           Data.List                  (find)
import           GHC.Generics               (Generic)

import qualified Data.Barbie                as B

import           Options.Harg.Sources.Types
import           Options.Harg.Types


-- | Source that enables a parser to read options from environment variables.
data EnvSource (f :: Type -> Type) = EnvSource
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

-- | Value of 'EnvSource', which is an association list between environment
-- variable names and values (strings).
newtype EnvSourceVal = EnvSourceVal Environment

instance GetSource EnvSource f where
  type SourceVal EnvSource = EnvSourceVal
  getSource HargCtx{..} _
    = pure (EnvSourceVal _hcEnv)

instance
    ( B.FunctorB a
    , B.TraversableB a
    ) => RunSource EnvSourceVal a where
  runSource (EnvSourceVal e) opt
    = [runEnvVarSource e opt]

-- | Try to get a value from the environment variable association list.
lookupEnv
  :: Environment
  -> String
  -> Maybe String
lookupEnv env x
  = snd <$> find ((== x) . fst) env

runEnvVarSource
  :: forall a f.
     ( B.FunctorB a
     , B.TraversableB a
     , Applicative f
     )
  => Environment
  -> a (Compose Opt f)
  -> Either SourceRunError (a (Compose SourceRunResult f))
runEnvVarSource env
  = B.btraverse go
  where
    go
      :: Compose Opt f x
      -> Either SourceRunError (Compose SourceRunResult f x)
    go (Compose opt@Opt{..})
      = maybe toNotFound (parse . lookupEnv env) _optEnvVar
      where
        parse
          = maybe toNotFound (either toErr toParsed . _optReader)
        toNotFound
          = Right $ Compose $ pure <$> OptNotFound
        toErr
          = Left . SourceRunError (Just (SomeOpt opt)) "EnvSource"
        toParsed
          = Right . Compose . OptParsed
