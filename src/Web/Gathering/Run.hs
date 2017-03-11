{- | The entry point to the application.

It will configure the app by parsing the command line arguments
and will execute the app according to a command which can be:

- http
- https
- both
- add-admin
- rem-admin
- del-user

more information about those in the Web.Gathering.Config file which
is responsible about parsing arguments and configuration

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

import System.Exit
import Control.Exception
import Data.Text (pack, unpack, Text)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings, tlsSettingsChain)
import Data.ByteString (ByteString)

import Hasql.Connection (Connection, acquire, release)
import qualified Hasql.Session as Sql (run)

import Web.Spock
import Web.Spock.Config


-- | This is the entry point of the application
--
run :: IO ()
run = do
  -- get configuration
  (conf, cmd) <- parseArgs
  let connstr = cfgDbConnStr conf

  -- Run a command or get the serve mode
  mode <- case cmd of
    Serve m ->
      pure m

    Cmd c -> do
      runCmd connstr c
      exitSuccess

  -- run logger
  (AppState conf mode -> state) <- runDefaultLogger

  -- Log configuration
  L.put (appLogger state) (pack $ show (conf, cmd))

  -- Run background workers
  void $ forkIO $ newEventsWorker state
  void $ forkIO $ eventRemindersWorker state
  void $ forkIO $ cleanerWorker state

  -- Run the spock app
  spockCfg <- (\cfg -> cfg { spc_csrfProtection = True })
          <$> defaultSpockCfg EmptySession (PCConn $ hasqlPool connstr) state

  case appMode state of
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
    settings
    (setPort (tlsPort tls) defaultSettings)
    spockApp

  where
    settings = case tlsChain tls of
      Nothing ->
         tlsSettings (tlsCert tls) (tlsKey tls)
      Just ch ->
         tlsSettingsChain (tlsCert tls) [ch] (tlsKey tls)

-- | Run other commands
runCmd :: ByteString -> Cmd -> IO ()
runCmd connStr cmd = do
  mConn <- acquire connStr
  case mConn of
    Right conn -> do
      let
        go = flip Sql.run conn . runWriteTransaction $
          case cmd of
            AddAdmin u -> do
              changeAdminForUser True u
            RemAdmin u ->
              changeAdminForUser False u
            DelUser  u ->
              deleteUser u

      result <- go `catch` \(e :: SomeException) -> release conn *> die (show e)
      report result

    Left ex -> do
      die ("Command Error: " ++ show ex)

-- | Report a commands result
report :: Show e => Either e (Either Text ()) -> IO ()
report = \case
  Right (Right ()) ->
    putStrLn "Done."
  Right (Left ex) ->
    die (unpack ex)
  Left ex ->
    die (show ex)
