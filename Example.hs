{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Example where

import           Data.Function         ((&))
import           Data.Functor.Identity (Identity (..))
import           GHC.Generics          (Generic)
import           System.Environment    (setEnv)

import           Options.Harg

mainSubparser :: IO ()
mainSubparser = do
  conf <- execOpt configOpt
  foldF conf
    (
      \(db :* srv :* hh :* _)
        -> AppC <$> getNested db <*> getNested srv <*> getSingle hh
           & runIdentity
           & print
    )
    (
      \(db :* tst :* _)
         -> TestAppC <$> getNested db <*> getNested tst
            & runIdentity
            & print
    )

  -- or:

  -- case conf of
    -- HereF (db :* srv :* hh :* _)
      -- -> let ov
               -- = AppC
               -- <$> getNested db
               -- <*> getNested srv
               -- <*> getArg hh
         -- in ov
            -- & extractOpt
            -- & print

    -- ThereF (HereF (db :* tst :* _))
      -- -> let ov
               -- = TestAppC
               -- <$> getNested db
               -- <*> getNested tst
         -- in ov
            -- & extractOpt
            -- & print

mainParser :: IO ()
mainParser = do
  db :* srv :* hh :* _ <- execOpt appOpt
  let ov
        = AppC
        <$> getNested db
        <*> getNested srv
        <*> getSingle hh

  print $ runIdentity ov

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
  :* Single Int

appOpt :: AppConfig Opt
appOpt
  = dbConf :* srvConf :* single something :* HNilF
  where
    srvConf
      = nested @ServiceConfig
          ( toOpt
            $ option readParser
            & long "port"
            & help "Web service port"
            & def 5432
          )
          ( toOpt
            $ switch
            & long "log"
            & help "Whether to log"
            & envVar "LOG"
          )
    something
      = toOpt
        $ option readParser
        & long "smth"
        & envVar "SOMETHING"
        & help "Something?"

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
          ( toOpt $ argument strParser
            -- & long "dir"
            -- & short 'd'
            & metavar "TEST_DIR"
            & help "Some directory"
            & envVar "TEST_DIR"
          )
          ( toOpt $ switch
            & long "mock"
            & short 'm'
            & help "Whether to mock"
            & envVar "MOCK"
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
      ( toOpt
        $ option strParser
        & long "db-user"
        & short 'u'
        & help "Database user"
        & envVar "DB_USER"
      )
      ( toOpt
        $ option readParser
        & long "db-port"
        & short 'p'
        & help "Database port"
        & envVar "DB_PORT"
        & def 5432
      )
