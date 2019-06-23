{-# LANGUAGE BlockArguments #-}
module Main where

import           Data.Char                      (toUpper)
import           Data.Function                  ((&))
import           GHC.Generics                   (Generic)
import           System.Environment             (setEnv)
import           Text.Read                      (readMaybe)

import qualified Data.Aeson                     as JSON
import qualified Data.Barbie                    as B
import qualified Data.Validation                as V

import           Data.Origin.Options
import           Data.Origin.Options.Operations (getOpt)
import           Data.Origin.Options.Types      ( SomeOpt(..)
                                                , OptError(..)
                                                , OptInvalidDetail(..))

-- just for debugging
instance Show SomeOpt where
  show (SomeOpt Opt{..})
    = "SomeOpt " <> _optLong <> " "

-- deriving instance Show a => Show (OptValue a)
deriving instance Show OptError
deriving instance Show OptInvalidDetail

main :: IO ()
main = do
  -- db :* srv :* hh :* _ <- getOptions appOptProd
  -- let ov
  --       = AppConfig
  --       <$> getNested db
  --       <*> getNested srv
  --       <*> getArg hh

  -- execOpt ov >>= print

  conf <- getOptions optTaggedSum
  foldF conf
    (
      \(db :* srv :* hh :* _)
        -> AppConfig <$> getNested db <*> getNested srv <*> getArg hh
           & execOpt >>= print
    )
    (
      \(db :* tst :* _)
         -> TestAppConfig <$> getNested db <*> getNested tst
            & execOpt >>= print
    )
  -- foldF conf
    -- (\(db :* srv :* hh :* _) -> getNested db & execOpt >>= print)
    -- (\(db :* tst :* _) -> getNested tst & execOpt >>= print)
  -- case conf of
  --   HereF (db :* srv :* hh :* _)
  --     -> let ov
  --              = AppConfig
  --              <$> getNested db
  --              <*> getNested srv
  --              <*> getArg hh
  --        in ov & execOpt >>= print

  --   ThereF (HereF (db :* tst :* _))
  --     -> let ov
  --              = TestAppConfig
  --              <$> getNested db
  --              <*> getNested tst
  --        in execOpt ov >>= print

type AppConfigProd
  =  Nested DBConfig
  :* Nested ServiceConfig
  :* Arg Int

appOptProd :: AppConfigProd Opt
appOptProd
  = dbConf :* srvConf :* arg (mkReadOpt "hehe" Nothing) :* HNilF
  where
    srvConf
      = nested @ServiceConfig
          (mkReadOpt "port" Nothing)
          (mkReadOpt "log" (Just True))

type TestAppConfigProd
  =  Nested DBConfig
  :* Nested TestConfig

testAppOptProd :: TestAppConfigProd Opt
testAppOptProd
  = dbConf :* testConf :* HNilF
  where
    testConf
      = nested @TestConfig
          (mkStringOpt "dir" Nothing)
          (mkReadOpt "mock" (Just False))

type ConfigSum
  =   "app" :-> AppConfigProd
  :** "test" :-> TestAppConfigProd

optTaggedSum :: ConfigSum Opt
optTaggedSum
  = appOptProd :+ testAppOptProd :+ ANil

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

data AppConfig
  = AppConfig
      { _acDbConfig      :: DBConfig
      , _acServiceConfig :: ServiceConfig
      , _acArg         :: Int
      }
  deriving Show

data TestAppConfig
  = TestAppConfig
      { _tacDbConfig   :: DBConfig
      , _tacTestConfig :: TestConfig
      }
  deriving Show

mkStringOpt :: String -> Maybe String -> Opt String
mkStringOpt n d
  = Opt
      { _optLong    = n
      , _optShort   = Nothing
      , _optHelp    = "help for " <> n
      , _optMetavar = Nothing
      , _optEnvVar  = Just (map toUpper n)
      , _optDefault = d
      , _optParser  = pure
      }

mkReadOpt :: Read a => String -> Maybe a -> Opt a
mkReadOpt n d
  = Opt
      { _optLong    = n
      , _optShort   = Nothing
      , _optHelp    = "help for " <> n
      , _optMetavar = Nothing
      , _optEnvVar  = Just (map toUpper n)
      , _optDefault = d
      , _optParser  = parseWith readMaybe
      }

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
