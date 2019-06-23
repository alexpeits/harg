{-# LANGUAGE PatternSynonyms #-}
module Data.Origin.Options
  ( Opt (..)
  , OptValue
  , Barbie (..)
  , option
  , optShort
  , optHelp
  , optMetavar
  , optEnvVar
  , optDefault
  , getOptions
  , Arg (..)
  , arg
  , Nested (..)
  , nested
  , getNested

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
import           Data.Origin.Options.Helpers
import           Data.Origin.Options.Operations
import           Data.Origin.Options.Pretty
import           Data.Origin.Options.Types
