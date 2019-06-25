{-# LANGUAGE PatternSynonyms #-}
module Data.Harg
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

import           Data.Harg.Het.AssocList
import           Data.Harg.Het.HList
import           Data.Harg.Het.Variant
import           Data.Harg.Options.Construct
import           Data.Harg.Options.Helpers
import           Data.Harg.Options.Operations
import           Data.Harg.Options.Pretty
import           Data.Harg.Options.Types
