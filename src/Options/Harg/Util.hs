{-# LANGUAGE RankNTypes #-}
module Options.Harg.Util where

import qualified Control.Exception          as Exc
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Functor.Compose       (Compose (..))
import           Data.Functor.Const         (Const(..))
import           System.Directory           (doesFileExist)
import           System.Exit                (exitFailure)

import qualified Data.Barbie                as B

import           Options.Harg.Het.HList
import           Options.Harg.Types


compose
  :: forall f g a.
      ( Functor f
      , B.FunctorB a
      )
  => (forall x. x -> g x)
  -> a f
  -> a (Compose f g)
compose to
  = B.bmap (Compose . fmap to)

-- | Convert an option parser into a dummy parser. A dummy option parser always
-- succeeds because options always have a default value (a monoid is used
-- here). This is useful because we want to run the parser together with the
-- configuration parser once in order to gather JSON file paths etc., which
-- means that we still need @--help@ to work.
toDummyOpts
  :: forall m a.
     ( B.FunctorB a
     , Monoid m
     )
  => a Opt
  -> a (Compose Opt (Const m))
toDummyOpts
  = B.bmap toDummy
  where
    toDummy opt
      = Compose
      $ Const
      <$> opt
            { _optDefault = Just mempty
            , _optReader  = pure . const mempty
            , _optType
                = case _optType opt of
                    OptionOptType   -> OptionOptType
                    FlagOptType _   -> FlagOptType mempty
                    ArgumentOptType -> ArgumentOptType
            }

-- | Convert an association list of options in to dummy ones.
allToDummyOpts
  :: forall m ts xs.
     ( Monoid m
     , MapAssocList xs
     )
  => AssocListF ts xs Opt
  -> AssocListF ts xs (Compose Opt (Const m))
allToDummyOpts
  = mapAssocList toDummyOpts

printErrAndExit
  :: forall a.
     String
  -> IO a
printErrAndExit
  = (>> exitFailure) . putStrLn

readFileWith
  :: (FilePath -> IO a)
  -> FilePath
  -> IO a
readFileWith f path
  = do
      exists <- doesFileExist path
      if exists
        then readFile_
        else printErrAndExit ("File not found: " <> path)
  where
    readFile_
      = f path
          `Exc.catch` (printErrAndExit . showExc)

    showExc :: Exc.IOException -> String
    showExc exc
      = "Could not read file " <> path <> ": " <> Exc.displayException exc

readFileLBS
  :: FilePath
  -> IO LBS.ByteString
readFileLBS
  = readFileWith LBS.readFile

readFileBS
  :: FilePath
  -> IO BS.ByteString
readFileBS
  = readFileWith BS.readFile
