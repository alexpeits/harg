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
    B.FunctorB a => RunSource EnvSourceVal a where
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
     , Applicative f
     )
  => Environment
  -> a (Compose Opt f)
  -> Either SourceRunError (a (Compose SourceRunResult f))
runEnvVarSource env
  = Right . B.bmap go
  where
    go :: Compose Opt f x -> Compose SourceRunResult f x
    go (Compose opt@Opt{..})
      = case _optEnvVar of
          Nothing
            -> Compose $ pure <$> OptNotFound
          Just envVar
            -> Compose $ maybe OptNotFound tryParse (lookupEnv env envVar)
      where
        tryParse
          = either
              (OptFoundNoParse . toOptError opt "EnvSource")
              OptParsed
          . _optReader
