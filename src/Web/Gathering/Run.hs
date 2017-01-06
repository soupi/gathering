-- | Run app according to configuration

module Web.Gathering.Run where

import Web.Gathering.Types
import Web.Gathering.Config

import Data.Monoid
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Web.Spock
import Web.Spock.Config
import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import qualified Data.Text as T

run :: IO ()
run = do
  (config, cmd) <- parseArgs
  print (config, cmd)
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState config)
  case cmd of
    HTTP port ->
      runSpock port (spock spockCfg app)
    HTTPS tls -> do
      runHttps spockCfg tls
    Both port tls -> do
      void $ forkIO $ runSpock port (spock spockCfg app)
      runHttps spockCfg tls

runHttps :: SpockCfg () MySession AppState -> TLSConfig -> IO ()
runHttps spockCfg tls = do
  spockApp <- spockAsApp (spock spockCfg app)
  runTLS
    (tlsSettings (tlsKey tls) (tlsCert tls))
    (setPort (tlsPort tls) defaultSettings)
    spockApp

app :: SpockM () MySession AppState ()
app = do
  get root $ text "Hello World!"
  get ("hello" <//> var) $ \name -> do
    AppState cfg <- getState
    text ("Hello " <> name <> ", welcome to " <> T.pack (show $ cfgTitle cfg))
