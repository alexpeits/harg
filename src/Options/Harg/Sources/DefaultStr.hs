{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Options.Harg.Sources.DefaultStr
  ( DefaultStrSource (..),
  )
where

import qualified Barbies as B
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import GHC.Generics (Generic)
import Options.Harg.Sources.Types
import Options.Harg.Types

-- | Source that enables a parser to read options from defaults that are provided
-- as strings (unparsed).
data DefaultStrSource (f :: Type -> Type) = DefaultStrSource
  deriving (Generic, B.FunctorB, B.TraversableB, B.ApplicativeB)

-- | Value of 'DefaultStrSource' is a dummy value, as the default string option can
-- be found inside the 'Opt' ('_optDefaultStr').
data DefaultStrSourceVal = DefaultStrSourceVal

instance GetSource DefaultStrSource f where
  type SourceVal DefaultStrSource = DefaultStrSourceVal
  getSource HargCtx {} _ =
    pure DefaultStrSourceVal

instance
  ( B.FunctorB a,
    B.TraversableB a
  ) =>
  RunSource DefaultStrSourceVal a
  where
  runSource DefaultStrSourceVal opt =
    [runDefaultStrSource opt]

-- TODO: this looks very similar to EnvSource, perhaps unify
runDefaultStrSource ::
  forall a f.
  ( B.FunctorB a,
    B.TraversableB a,
    Applicative f
  ) =>
  a (Compose Opt f) ->
  Either SourceRunError (a (Compose SourceRunResult f))
runDefaultStrSource =
  B.btraverse go
  where
    go ::
      Compose Opt f x ->
      Either SourceRunError (Compose SourceRunResult f x)
    go (Compose opt@Opt {..}) =
      maybe toNotFound parse _optDefaultStr
      where
        parse =
          either toErr toParsed . _optReader
        toNotFound =
          Right $ Compose $ pure <$> OptNotFound
        toErr =
          Left . sourceRunError opt "DefaultStrSource"
        toParsed =
          Right . Compose . OptParsed
