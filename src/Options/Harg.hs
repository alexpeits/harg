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

  , execParserDef
  , execParserDefPure
  -- , execOpt
  , execOpt
  , execOptSubcommand
  -- , execOptPure
  , Env (..)
  , Jason (..)

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

  , dummyOpt
  , jsonOpt
  ) where

import Options.Harg.Construct
import Options.Harg.Het.HList
import Options.Harg.Het.Prod
import Options.Harg.Het.Variant
import Options.Harg.Operations
import Options.Harg.Types
import Options.Harg.Sources
