{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Example where

import           Data.Function         ((&))
import           Data.Functor.Identity (Identity (..))
import           Data.Kind             (Type)
import           GHC.Generics          (Generic)
import           System.Environment    (setEnv)

import qualified Data.Aeson            as JSON
import qualified Data.Barbie           as B
import qualified Data.ByteString.Lazy  as BS

import           Options.Harg


jsonOpt :: Opt FilePath
jsonOpt
  = toOpt
    $ option strParser
    & optLong "json-config"
    & optShort 'j'
    & optHelp "JSON config"

yamlOpt :: Opt FilePath
yamlOpt
  = toOpt
    $ option strParser
    & optLong "yaml-config"
    & optShort 'y'
    & optHelp "YAML config"

type SourceOpt
  =  EnvSource
  :* JSONSource
  :* YAMLSource

srcOpt :: SourceOpt Opt
srcOpt
  =  EnvSource
  :* JSONSource jsonOpt
  :* YAMLSource yamlOpt

mainSubparser :: IO ()
mainSubparser = do
  conf <- execCommands srcOpt configOpt
  fromVariantF conf
    (
      \(db :* srv :* smth)
        -> AppC
           <$> getNested (unTagged db)
           <*> getNested (unTagged srv)
           <*> getSingle (unTagged smth)
           & runIdentity
           & print
    )
    (
      \(db :* tst)
         -> TestAppC
            <$> getNested (unTagged db)
            <*> getNested (unTagged tst)
            & runIdentity
            & print
    )

mainParser :: IO ()
mainParser = do
  db :* srv :* smth <- execOpt srcOpt appOpt
  let
    res
      = AppC
      <$> getNested (unTagged db)
      <*> getNested (unTagged srv)
      <*> getSingle (unTagged smth)
  print $ runIdentity res

main :: IO ()
main
  = mainParser
  -- = mainSubparser

data AppC
  = AppC
      { _acDbConfig      :: DBConfig
      , _acServiceConfig :: ServiceConfig
      , _acSomething     :: Maybe Int
      }
  deriving Show

type AppConfig
  =  Tagged "db" (Nested DBConfig)
  :* Tagged "srv" (Nested ServiceConfig)
  :* Tagged "smth" (Single (Maybe Int))

appOpt :: AppConfig Opt
appOpt
  =  Tagged dbConf
  :* Tagged srvConf
  :* Tagged (single something)
  where
    something
      = optionWith readParser
          ( optLong "smth"
          . optEnvVar "SOMETHING"
          . optHelp "Something?"
          . optOptional
          )

data TestAppC
  = TestAppC
      { _tacDbConfig   :: DBConfig
      , _tacTestConfig :: TestConfig
      }
  deriving Show

type TestAppConfig
  =  Tagged "db" (Nested DBConfig)
  :* Tagged "tst" (Nested TestConfig)

testAppOpt :: TestAppConfig Opt
testAppOpt
  =  Tagged dbConf
  :* Tagged testConf
  where
    testConf
      = nested @TestConfig
          ( argumentWith strParser
              ( optMetavar "TEST_DIR"
              . optHelp "Some directory"
              . optEnvVar "TEST_DIR"
              . optOptional
              )
          )
          ( toOpt $ switch
            & optLong "mock"
            & optShort 'm'
            & optHelp "Whether to mock"
            & optEnvVar "MOCK"
          )

type Config
  =  "app" :-> AppConfig
  :+ "test" :-> TestAppConfig

configOpt :: Config Opt
configOpt
  = appOpt :+ testAppOpt :+ ANil

data DBConfig
  = DBConfig
      { _dbUser :: String
      , _dbPort :: Int
      }
  deriving (Show, Generic)

dbConf :: Nested DBConfig Opt
dbConf
  = nested @DBConfig
      ( toOpt
        $ option strParser
        & optLong "db-user"
        & optShort 'u'
        & optHelp "Database user"
        & optEnvVar "DB_USER"
      )
      ( toOpt
        $ option readParser
        & optLong "db-port"
        & optShort 'p'
        & optHelp "Database port"
        & optEnvVar "DB_PORT"
        & optDefault 5432
      )

data ServiceConfig
  = ServiceConfig
      { _srvPort :: Int
      , _srvLog  :: Bool
      }
  deriving (Show, Generic)

srvConf :: Nested ServiceConfig Opt
srvConf
  = nested @ServiceConfig
      ( toOpt
        $ option readParser
        & optLong "port"
        & optHelp "Web service port"
        & optDefault 5432
      )
      ( toOpt
        $ switch
        & optLong "log"
        & optHelp "Whether to log"
        & optEnvVar "LOG"
      )

data TestConfig
  = TestConfig
      { _tDir  :: Maybe String
      , _tMock :: Bool
      }
  deriving (Show, Generic)
