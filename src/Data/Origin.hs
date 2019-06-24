{-# LANGUAGE PatternSynonyms #-}
module Data.Origin
  ( Opt
  , OptValue
  , Barbie (..)
  , getOptions
  , Arg (..)
  , arg
  , Nested (..)
  , nested
  , getNested

  , mkOpt
  , option
  , optionWith
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
  , HListF (..)
  , AssocListF (..)

  , foldF

  , (:*)
  , pattern (:*)
  , (:**)
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
