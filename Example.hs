{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveGeneric      #-}
module Example where

import           Data.Function                  ((&))
import           GHC.Generics                   (Generic)

import           Data.Origin

import qualified Options.Applicative as OA

data MyApp = MyApp { appGreet :: String
                   , appSuperExcited :: String
                   }

runWithOptions :: MyApp -> IO ()
runWithOptions opts =
  putStrLn $ transform $
    "Merry Christmas, " ++ appGreet opts ++ "!"

  where
    transform = (appSuperExcited opts <>)

main :: IO ()
main = OA.execParser opts >>= runWithOptions
  where
    parser = MyApp <$> OA.argument OA.str (OA.metavar "NAME")
                   <*> OA.strOption (OA.short 'e' <>
                                     OA.long "excited" <>
                                     OA.help "Run in excited mode")
    opts = OA.info (OA.helper <*> parser) mempty

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

_main :: IO ()
_main
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
          ( mkOpt
            $ option "port" readParser
            & optHelp "Web service port"
          )
          ( mkOpt
            $ switch "log"
            & optHelp "Whether to log"
          )
    something
      = mkOpt
        $ option "smth" readParser
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
          ( mkOpt $ option "dir" strParser
            & optShort 'd'
            & optHelp "Some directory"
            & optEnvVar "TEST_DIR"
          )
          ( mkOpt $ switch "mock"
            & optHelp "Whether to mock"
            & optEnvVar "MOCK"
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
      ( mkOpt
        $ option "db-user" strParser
        & optShort 'u'
        & optHelp "Database user"
        & optEnvVar "DB_USER"
      )
      ( mkOpt
        $ option "db-port" readParser
        & optShort 'p'
        & optHelp "Database port"
        & optEnvVar "DB_PORT"
        & optDefault 5432
      )
