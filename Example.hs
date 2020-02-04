{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
module Example where

import Data.Function         ((&))
import Data.Functor.Identity (Identity (..))
import GHC.Generics          (Generic)

import Options.Harg


jsonOpt :: Opt ConfigFile
jsonOpt
  = option strParser
      ( long "json-config"
      . short 'j'
      . help "JSON config"
      . defaultVal NoConfigFile
      )

yamlOpt :: Opt ConfigFile
yamlOpt
  = option strParser
      ( long "yaml-config"
      . short 'y'
      . help "YAML config"
      . defaultVal NoConfigFile
      )

type SourceOpt
  =  JSONSource
  :* YAMLSource
  :* EnvSource

srcOpt :: SourceOpt Opt
srcOpt
  =  JSONSource jsonOpt
  :* YAMLSource yamlOpt
  :* EnvSource

-- mainSubparser :: IO ()
-- mainSubparser = do
--   conf <- execCommands srcOpt configOpt
--   fromVariantF conf
--     (
--       \(db :* srv :* smth :* manyStuff)
--         -> AppC
--            <$> getNested (unTagged db)
--            <*> getNested (unTagged srv)
--            <*> getSingle (unTagged smth)
--            <*> getSingle (unTagged manyStuff)
--            & runIdentity
--            & print
--     )
--     (
--       \(db :* tst)
--          -> TestAppC
--             <$> getNested (unTagged db)
--             <*> getNested (unTagged tst)
--             & runIdentity
--             & print
--     )

mainParser :: IO ()
mainParser = do
  ctx <- getCtx
  db :* srv :* smth :* manyStuff <- execOptWithConf ctx defaultSources appOpt
  -- db :* srv :* smth :* manyStuff <- execOptDef appOpt
  let
    res
      = AppC
      <$> getNested (unTagged db)
      <*> getNested (unTagged srv)
      <*> getSingle (unTagged smth)
      <*> getSingle (unTagged manyStuff)
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
      , _acManyStuff     :: [Int]
      }
  deriving Show

type AppConfig
  =  Tagged "db" (Nested DBConfig)
  :* Tagged "srv" (Nested ServiceConfig)
  :* Tagged "smth" (Single (Maybe Int))
  :* Tagged "manyStuff" (Single [Int])

appOpt :: AppConfig Opt
appOpt
  =  Tagged dbConf
  :* Tagged srvConf
  :* Tagged (single something)
  :* Tagged (single manyStuff)
  where
    something
      = option readParser
          ( long "smth"
          . envVar "SOMETHING"
          . help "Something?"
          . optional
          )
    manyStuff
      = option (manyParser "," readParser)
          ( long "many"
          . envVar "MANY"
          . help "Many stuff"
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
          ( argument strParser
              ( metavar "TEST_DIR"
              . help "Some directory"
              . envVar "TEST_DIR"
              . optional
              )
          )
          ( switch
              ( long "mock"
              . short 'm'
              . help "Whether to mock"
              . envVar "MOCK"
              )
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
      ( option strParser
          ( long "db-user"
          . short 'u'
          . help "Database user"
          . envVar "DB_USER"
          )
      )
      ( option readParser
          ( long "db-port"
          . short 'p'
          . help "Database port"
          . envVar "DB_PORT"
          . defaultVal 5432
          )
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
      ( option readParser
          ( long "port"
          . help "Web service port"
          . defaultStr "25000"
          )
      )
      ( switch
          ( long "log"
          . help "Whether to log"
          . envVar "LOG"
          )
      )

data TestConfig
  = TestConfig
      { _tDir  :: Maybe String
      , _tMock :: Bool
      }
  deriving (Show, Generic)
