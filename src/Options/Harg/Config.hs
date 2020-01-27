module Options.Harg.Config
  ( mkConfigParser
  , getConfig
  ) where

import           Data.Functor.Compose       (Compose (..))
import           Data.Kind                  (Type)

import qualified Data.Barbie                as B
import qualified Options.Applicative        as Optparse

import           Options.Harg.Cmdline       (mkOptparseParser)
import           Options.Harg.Sources       (accumSourceResults)
import           Options.Harg.Sources.Env   (EnvSourceVal(..))
import           Options.Harg.Sources.Types
import           Options.Harg.Types


-- | Create a 'Optparse.Parser' for the configuration option parser, using
-- 'EnvSource' as the only source.
mkConfigParser
  :: forall f c.
     ( Applicative f
     , B.TraversableB c
     , B.ProductB c
     )
  => HargCtx
  -> c (Compose Opt f)
  -> Optparse.Parser (c f)
mkConfigParser HargCtx{..} conf
  = mkOptparseParser envC conf
  where
    (_, envC)
      = accumSourceResults
        $ runSource (EnvSourceVal _hcEnv) conf

-- | Run two option parsers in parallel and return the result of the
-- first one. This is used with the configuration parser being the first
-- argument, and the target option parser that has been converted to
-- the dummy parser using 'Options.Harg.Util.toDummyOpts' as the second
-- one.
getConfig
  :: HargCtx
  -> Optparse.Parser (c (f :: Type -> Type))
  -> Optparse.Parser (a (g :: Type -> Type))
  -> IO (c f)
getConfig HargCtx{..} confParser optParser = do
  let
    parser
      = (,) <$> confParser <*> optParser
    parserInfo
      = Optparse.info (Optparse.helper <*> parser) mempty
    res
      = Optparse.execParserPure Optparse.defaultPrefs parserInfo _hcArgs
  fst <$> Optparse.handleParseResult res
