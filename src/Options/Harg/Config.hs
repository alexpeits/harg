module Options.Harg.Config where

import           Data.Functor.Compose       (Compose (..))

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
  :: ( Applicative f
     , Applicative g
     )
  => Optparse.Parser (c f)
  -> Optparse.Parser (a g)
  -> IO (c f)
getConfig confParser optParser
  = do
      let
        allParser
          = (,) <$> confParser <*> optParser
        pInfo
          = Optparse.info (Optparse.helper <*> allParser) mempty
      fst <$> Optparse.execParser pInfo
