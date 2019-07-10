{-# LANGUAGE RankNTypes #-}
module Options.Harg.Util where

import qualified Control.Exception          as Exc
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Functor.Compose       (Compose (..))
import           Data.Functor.Const         (Const(..))
import           Data.Functor.Product       (Product (..))
import           System.Directory           (doesFileExist)
import           System.Exit                (exitFailure)

import qualified Data.Barbie                as B

import           Options.Harg.Het.HList
import           Options.Harg.Types


bpairwise
  :: forall a f g h. B.ProductB a
  => (forall x. f x -> g x -> h x)
  -> a f
  -> a g
  -> a h
bpairwise f xs ys
  = B.bmap (\(Pair x y) -> f x y) (B.bprod xs ys)

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

allToDummyOpts
  :: forall m ts xs.
     ( Monoid m
     , MapAssocList xs
     )
  => AssocListF ts xs Opt
  -> AssocListF ts xs (Compose Opt (Const m))
allToDummyOpts
  = mapAssocList toDummyOpts

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
