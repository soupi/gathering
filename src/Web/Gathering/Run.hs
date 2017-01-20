{- | The entry point to the application.

It will configure the app by parsing the command line arguments
and will execute the app according to command

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Run where

import Web.Gathering.Types
import Web.Gathering.Config
import Web.Gathering.Database
import Web.Gathering.Router
import Web.Gathering.Workers.SendEmails
import Web.Gathering.Workers.Cleaner
import Web.Gathering.Workers.Logger
import qualified Web.Gathering.Workers.Logger as L

import Data.Text (pack)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)

import Hasql.Connection (Connection)

import Web.Spock
import Web.Spock.Config


-- | This is the entry point of the application
--
--   It will parse the arguments to get the configuration and will
--   Initialize the application. Then it will run the app according
--   To the command mode: http, https or both.
--
run :: IO ()
run = do
  (conf, cmd) <- parseArgs

  -- logger
  (AppState conf cmd -> state) <- runDefaultLogger

  L.put (appLogger state) (pack $ show (conf, cmd))

  -- Background workers
  void $ forkIO $ newEventsWorker state
  void $ forkIO $ cleanerWorker state

  -- app
  let connstr = cfgDbConnStr conf
  spockCfg <- (\cfg -> cfg { spc_csrfProtection = True })
          <$> defaultSpockCfg EmptySession (PCConn $ hasqlPool connstr) state

  case appCommand state of
    HTTP port ->
      runSpock port (spock spockCfg appRouter)
    HTTPS tls -> do
      runHttps spockCfg tls
    Both port tls -> do
      void $ forkIO $ runSpock port (spock spockCfg appRouter)
      runHttps spockCfg tls



-- | Run the spock app with HTTPS
runHttps :: SpockCfg Connection MySession AppState -> TLSConfig -> IO ()
runHttps spockCfg tls = do
  spockApp <- spockAsApp (spock spockCfg appRouter)
  runTLS
    (tlsSettings (tlsKey tls) (tlsCert tls))
    (setPort (tlsPort tls) defaultSettings)
    spockApp

