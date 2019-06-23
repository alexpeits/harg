{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveGeneric      #-}
module Example where

import           Data.Function                  ((&))
import           GHC.Generics                   (Generic)

import           Data.Origin

mainSubparser :: IO ()
mainSubparser = do
  conf <- getOptions configOpt
  foldF conf
    (
      \(db :* srv :* hh :* _)
        -> AppC <$> getNested db <*> getNested srv <*> getArg hh
           & execOpt >>= print
    )
    (
      \(db :* tst :* _)
         -> TestAppC <$> getNested db <*> getNested tst
            & execOpt >>= print
    )

  -- or:

  -- case conf of
  --   HereF (db :* srv :* hh :* _)
  --     -> let ov
  --              = AppC
  --              <$> getNested db
  --              <*> getNested srv
  --              <*> getArg hh
  --        in ov & execOpt >>= print

  --   ThereF (HereF (db :* tst :* _))
  --     -> let ov
  --              = TestAppC
  --              <$> getNested db
  --              <*> getNested tst
  --        in execOpt ov >>= print

mainParser :: IO ()
mainParser = do
  db :* srv :* hh :* _ <- getOptions appOpt
  let ov
        = AppC
        <$> getNested db
        <*> getNested srv
        <*> getArg hh

  execOpt ov >>= print

main :: IO ()
main
  -- = mainParser
  = mainSubparser

data AppC
  = AppC
      { _acDbConfig      :: DBConfig
      , _acServiceConfig :: ServiceConfig
      , _acSomething     :: Int
      }
  deriving Show

type AppConfig
  =  Nested DBConfig
  :* Nested ServiceConfig
  :* Arg Int

appOpt :: AppConfig Opt
appOpt
  = dbConf :* srvConf :* arg something :* HNilF
  where
    srvConf
      = nested @ServiceConfig
          ( option "port" readParser
            & optHelp "Web service port"
          )
          ( option "log" readParser
            & optHelp "Whether to log"
            & optDefault True
          )
    something
      = option "smth" readParser
        & optHelp "Something?"

data TestAppC
  = TestAppC
      { _tacDbConfig   :: DBConfig
      , _tacTestConfig :: TestConfig
      }
  deriving Show

type TestAppConfig
  =  Nested DBConfig
  :* Nested TestConfig

testAppOpt :: TestAppConfig Opt
testAppOpt
  = dbConf :* testConf :* HNilF
  where
    testConf
      = nested @TestConfig
          ( option "dir" strParser
            & optShort 'd'
            & optHelp "Some directory"
            & optEnvVar "TEST_DIR"
          )
          ( option "mock" readParser
            & optHelp "Whether to mock"
            & optDefault False
          )

type Config
  =   "app" :-> AppConfig
  :** "test" :-> TestAppConfig

configOpt :: Config Opt
configOpt
  = appOpt :+ testAppOpt :+ ANil

data DBConfig
  = DBConfig
      { _dbUser :: String
      , _dbPort :: Int
      }
  deriving (Show, Generic)

data ServiceConfig
  = ServiceConfig
      { _srvPort :: Int
      , _srvLog  :: Bool
      }
  deriving (Show, Generic)

data TestConfig
  = TestConfig
      { _tDir  :: String
      , _tMock :: Bool
      }
  deriving (Show, Generic)


dbConf :: Nested DBConfig Opt
dbConf
  = nested @DBConfig
      ( option "db-user" strParser
        & optShort 'u'
        & optHelp "Database user"
        & optEnvVar "DB_USER"
      )
      ( option "db-port" readParser
        & optShort 'p'
        & optHelp "Database port"
        & optEnvVar "DB_PORT"
        & optDefault 5432
      )
