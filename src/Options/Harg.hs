{-# LANGUAGE PatternSynonyms #-}
module Options.Harg
  ( Opt
  , Single (..)
  , single
  , Nested (..)
  , nested
  , getNested

  , toOpt
  , option
  , optionWith
  , flag
  , flagWith
  , switch
  , switchWith
  , switch'
  , switchWith'
  , argument
  , argumentWith

  , optLong
  , optShort
  , optHelp
  , optMetavar
  , optEnvVar
  , optDefault

  , parseWith
  , readParser
  , strParser

  , execOpt
  , execOptDef
  , execCommands
  , execCommandsDef

  , EnvSource (..)
  , JSONSource (..)
  , defaultSources

  , (:*) (..)
  , Tagged (..)

  , VariantF (..)
  , pattern In1
  , pattern In2
  , pattern In3
  , pattern In4
  , pattern In5

  , foldF

  , AssocListF (..)
  , (:+)
  , pattern (:+)
  , (:->)
  ) where

import Options.Harg.Construct
import Options.Harg.Het.HList
import Options.Harg.Het.Prod
import Options.Harg.Het.Variant
import Options.Harg.Nested
import Options.Harg.Operations
import Options.Harg.Single
import Options.Harg.Sources
import Options.Harg.Sources.Env
import Options.Harg.Sources.JSON
import Options.Harg.Types
