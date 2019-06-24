{-# LANGUAGE PatternSynonyms #-}
module Data.Origin
  ( Opt
  , OptValue
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

  , ppError

  , optValue
  , execOpt
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

import           Data.Origin.Het.AssocList
import           Data.Origin.Het.HList
import           Data.Origin.Het.Variant
import           Data.Origin.Options.Construct
import           Data.Origin.Options.Helpers
import           Data.Origin.Options.Operations
import           Data.Origin.Options.Pretty
import           Data.Origin.Options.Types
