{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}
module Options.Harg.Sources.DefaultStr where

import           Data.Functor.Compose       (Compose (..))
import           Data.Kind                  (Type)
import           GHC.Generics               (Generic)

import qualified Data.Barbie                as B

import           Options.Harg.Sources.Types
import           Options.Harg.Types


-- | Source that enables a parser to read options from defaults that are provided
-- as strings (unparsed).
data DefaultStrSource (f :: Type -> Type) = DefaultStrSource
  deriving (Generic, B.FunctorB, B.TraversableB, B.ProductB)

-- | Value of 'DefaultStrSource' is a dummy value, as the default string option can
-- be found inside the 'Opt' ('_optDefaultStr').
data DefaultStrSourceVal = DefaultStrSourceVal

instance GetSource DefaultStrSource f where
  type SourceVal DefaultStrSource = DefaultStrSourceVal
  getSource HargCtx{..} _
    = pure DefaultStrSourceVal

instance
    B.FunctorB a => RunSource DefaultStrSourceVal a where
  runSource DefaultStrSourceVal opt
    = [runDefaultStrSource opt]

runDefaultStrSource
  :: forall a f.
     ( B.FunctorB a
     , Applicative f
     )
  => a (Compose Opt f)
  -> Either SourceRunError (a (Compose SourceRunResult f))
runDefaultStrSource
  = Right . B.bmap go
  where
    go :: Compose Opt f x -> Compose SourceRunResult f x
    go (Compose opt@Opt{..})
      = case _optDefaultStr of
          Nothing
            -> Compose $ pure <$> OptNotFound
          Just str
            -> Compose $ tryParse str
      where
        tryParse
          = either
              (OptFoundNoParse . toOptError opt "DefaultStrSource")
              OptParsed
          . _optReader
