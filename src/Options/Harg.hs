{-# LANGUAGE PatternSynonyms #-}
module Options.Harg
  ( -- * Summary
    -- $summary

    -- ** Option declaration
    option
  , optionWith
  , flag
  , flagWith
  , switch
  , switchWith
  , switch'
  , switchWith'
  , argument
  , argumentWith

  , Single (..)
  , single

  , Nested (..)
  , nested
  , getNested

  , AssocListF (..)
  , (:+)
  , pattern (:+)
  , (:->)

  , (:*) (..)
  , Tagged (..)

  -- ** Option modifiers
  , optLong
  , optShort
  , optHelp
  , optMetavar
  , optEnvVar
  , optDefault
  , optOptional
  , toOpt
  , Opt

  -- ** Option parsers
  , parseWith
  , readParser
  , strParser
  , boolParser

  -- ** Executing options
  , execOpt
  , execOptDef
  , execCommands
  , execCommandsDef

  -- ** Option sources
  , EnvSource (..)
  , JSONSource (..)
  , YAMLSource (..)
  , ConfigFile (..)
  , noSources
  , defaultSources

  -- ** Parser context
  , getCtx
  , ctxFromArgs
  , ctxFromEnv
  , pureCtx

  -- ** Variant
  , VariantF (..)
  , fromVariantF
  , pattern In1
  , pattern In2
  , pattern In3
  , pattern In4
  , pattern In5

  -- ** Re-exports
  , B.FunctorB
  , B.TraversableB
  , B.ProductB

  , HKD.HKD
  , HKD.build
  , HKD.construct
  ) where

import           Options.Harg.Construct
import           Options.Harg.Het.HList
import           Options.Harg.Het.Prod
import           Options.Harg.Het.Variant
import           Options.Harg.Nested
import           Options.Harg.Operations
import           Options.Harg.Single
import           Options.Harg.Sources
import           Options.Harg.Sources.Env
import           Options.Harg.Sources.JSON
import           Options.Harg.Sources.NoSource
import           Options.Harg.Sources.Types
import           Options.Harg.Sources.YAML
import           Options.Harg.Types

import qualified Data.Barbie                   as B
import qualified Data.Generic.HKD              as HKD


-- $summary
--
-- @harg@ is a wrapper around @optparse-applicative@ that allows blending
-- command-line configuration with environment variables, defaults as well as
-- other sources such as JSON or YAML files. Here are some very simple examples:
--
-- * Flat configuration type
--
-- @
--   data Config
--     = Config
--         { host :: String
--         , port :: Int
--         , log  :: Bool
--         , dir  :: Maybe String
--         }
--
--   -- Using 'HKD' from higgledy
--   configOpt :: HKD Config Opt
--   configOpt
--     = build @Config hostOpt portOpt logOpt dirOpt
--     where
--       hostOpt
--         = optionWith strParser
--             ( optLong \"host\"
--             . optShort \'h\'
--             . optHelp \"Hostname\"
--             . optEnvVar \"HOST_NAME\"
--             )
--       portOpt
--         = optionWith readParser
--             ( optLong \"port\"
--             . optShort \'p\'
--             . optHelp \"Port number\"
--             . optDefault 5432
--             )
--       logOpt
--         = switchWith
--             ( optLong \"log\"
--             . optHelp \"Whether to log or not\"
--             )
--       dirOpt
--         = argumentWith strParser
--             ( optHelp \"Some directory\"
--             . optEnvVar \"SOME_DIR\"
--             . optOptional
--             )
--
--   main :: IO Config
--   main = do
--     result <- execOpt defaultSources configOpt
--     pure $ runIdentity (construct result)
-- @
--
-- The above could also be:
--
-- @
--   type ConfigOpt
--     =  Single String
--     :* Single Int
--     :* Single Bool
--     :* Single String
--
--   configOpt :: ConfigOpt Opt
--   configOpt
--     = hostOpt :* portOpt :* logOpt :* dirOpt
--     where
--       ...
--
--   main :: IO Config
--   main = do
--     host :* port :* log :* dir <- execOpt defaultSources configOpt
--     pure
--       $ runIdentity
--       $ Config
--       \<$\> getSingle host
--       \<*\> getSingle port
--       \<*\> getSingle log
--       \<*\> getSingle dir
-- @
--
-- * Nested configuration type
--
-- @
--   data Config
--     = Config
--         { dbConfig :: DbConfig
--         , serverConfig :: ServerConfig
--         }
--
--   data DbConfig
--     = DbConfig
--         { dbHost :: String
--         , dbPort :: Int
--         }
--
--   data ServerConfig
--     = ServerConfig
--         { srvPort :: Int
--         , srvLog  :: Bool
--         }
--
--   type ConfigOpt
--     =  HKD DbConfig
--     :* HKD ServerConfig
--
--   configOpt :: ConfigOpt Opt
--   configOpt
--     = dbOpt :* srvOpt
--     where
--       dbOpt = build @DbConfig ...
--       srvOpt = build @ServerConfig ...
--
--   main :: IO Config
--   main = do
--     db :* srv <- execOpt defaultSources configOpt
--     pure
--       $ runIdentity
--       $ Config
--       \<$\> construct db
--       \<*\> construct srv
-- @
--
-- * Subparsers
--
-- @
--   data OneConfig = OneConfig ...
--   data OtherConfig = OtherConfig ...
--
--   data Config
--     =  "one" :-> OneConfig
--     :+ "other" :-> OtherConfig
--
--   configOpt :: Config Opt
--   configOpt
--     = oneOpt :+ otherOpt :+ ANil
--     where
--       oneOpt = ...
--       otherOpt = ...
--
--   main :: IO ()
--   main = do
--     result <- execOpt defaultSources configOpt
--     case result of
--       HereF one            -> runWithOne one
--       ThereF (HereF other) -> runWithOther other
--     where
--       runWithOne :: One -> IO ()
--       runWithOne = ...
--       runWithOther :: Other -> IO ()
--       runWithOther = ...
-- @
--
-- TODO: more (and better) examples
