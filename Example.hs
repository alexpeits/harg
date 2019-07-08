{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import qualified Data.Aeson            as JSON
import qualified Data.Barbie           as B
import qualified Data.ByteString.Lazy  as BS
import qualified Data.Generic.HKD      as HKD

mainSubparser :: IO ()
mainSubparser = do
  conf <- execCommands srcOpt configOpt
  foldF conf
    (
      \(db :* srv :* hh)
        -> AppC <$> getNested (unTagged db) <*> getNested (unTagged srv) <*> getSingle (unTagged hh)
           & runIdentity
           & print
    )
    (
      \(db :* tst)
         -> TestAppC <$> getNested (unTagged db) <*> getNested (unTagged tst)
            & runIdentity
            & print
    )

  -- or:

  -- case conf of
    -- HereF (db :* srv :* hh)
      -- -> let ov
               -- = AppC
               -- <$> getNested db
               -- <*> getNested srv
               -- <*> getArg hh
         -- in ov
            -- & runIdentity
            -- & print

    -- ThereF (HereF (db :* tst))
      -- -> let ov
               -- = TestAppC
               -- <$> getNested db
               -- <*> getNested tst
         -- in ov
            -- & runIdentity
            -- & print

srcOpt :: (Env :* Jason) Opt
srcOpt = Env :* Jason (jsonOpt "j1")

srcOpt' :: (Jason :* Env) Opt
srcOpt' = Jason (jsonOpt "j1") :* Env

mainParser :: IO ()
mainParser = do
  db :* srv :* hh <- execOpt srcOpt appOpt
  let ov
        = AppC
        <$> getNested (unTagged db)
        <*> getNested (unTagged srv)
        <*> getSingle (unTagged hh)

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
  =  Tagged "db" (Nested DBConfig)
  :* Tagged "srv" (Nested ServiceConfig)
  :* Tagged "smth" (Single Int)

appOpt :: AppConfig Opt
appOpt
  =  Tagged dbConf
  :* Tagged srvConf
  :* Tagged (single something)
  where
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
    something
      = toOpt
        $ option readParser
        & optLong "smth"
        & optEnvVar "SOMETHING"
        & optHelp "Something?"

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
          ( toOpt $ argument strParser
            -- & optLong "dir"
            -- & optShort 'd'
            & optMetavar "TEST_DIR"
            & optHelp "Some directory"
            & optEnvVar "TEST_DIR"
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

data TestConfig'
  = TestConfig'
      { tDir  :: String
      , tMock :: Booly
      }
  deriving (Show, Generic)

newtype Booly = Booly { getBooly :: Bool }
  deriving (Show)

foo :: BS.ByteString -> Maybe (HKD.HKD TestConfig Maybe)
foo = JSON.decode

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
