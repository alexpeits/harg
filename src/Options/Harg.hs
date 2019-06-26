{-# LANGUAGE PatternSynonyms #-}
module Options.Harg
  ( Opt
  , Barbie (..)
  , getOptions
  , Single (..)
  , single
  , Nested (..)
  , nested
  , getNested

  , mkOpt
  , arg
  , argWith
  , flag
  , flagWith
  , switch
  , switchWith
  , switch'
  , switchWith'
  , optShort
  , optHelp
  , optMetavar
  , optEnvVar
  , optDefault

  , extractOpt
  , parseWith
  , readParser
  , strParser

  , VariantF (..)
  , pattern In1
  , pattern In2
  , pattern In3
  , pattern In4
  , pattern In5

  , HListF (..)

  , AssocListF (..)

  , foldF

  , (:*)
  , pattern (:*)
  , (:+)
  , pattern (:+)
  , (:->)
  ) where

import           Options.Harg.Het.AssocList
import           Options.Harg.Het.HList
import           Options.Harg.Het.Variant
import           Options.Harg.Construct
import           Options.Harg.Helpers
import           Options.Harg.Operations
import           Options.Harg.Types
