module Options.Harg.Config where

import           Data.Functor.Compose       (Compose (..))
import           Data.Kind                  (Type)

import qualified Data.Barbie                as B
import qualified Options.Applicative        as Optparse

import           Options.Harg.Cmdline
import           Options.Harg.Sources
import           Options.Harg.Sources.Env
import           Options.Harg.Sources.Types
import           Options.Harg.Types


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
  = let
      (_, envC)
        = accumSourceResults
        $ runSource (EnvSourceVal _hcEnv) conf
    in mkOptparseParser envC conf

getConfig
  :: HargCtx
  -> Optparse.Parser (c (f :: Type -> Type))
  -> Optparse.Parser (a (g :: Type -> Type))
  -> IO (c f)
getConfig HargCtx{..} confParser optParser
  = do
      let
        parser
          = (,) <$> confParser <*> optParser
        parserInfo
          = Optparse.info (Optparse.helper <*> parser) mempty
        res
          = Optparse.execParserPure Optparse.defaultPrefs parserInfo _hcArgs
      fst <$> Optparse.handleParseResult res
