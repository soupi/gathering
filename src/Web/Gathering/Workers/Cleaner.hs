{- | A worker that runs in the background and cleans old data from tables

-}


module Web.Gathering.Workers.Cleaner
  ( cleanerWorker
  )
where

import Data.Text (pack)
import Turtle (forever, sleep)
import Hasql.Session
import Hasql.Connection

import Web.Gathering.Database
import Web.Gathering.Types
import Web.Gathering.Config
import Web.Gathering.Workers.Logger

cleanerWorker :: AppState -> IO ()
cleanerWorker state = forever $ do
  mConn <- acquire (cfgDbConnStr $ appConfig state)

  case mConn of
    Right conn -> do
      put (appLogger state) "Cleaner starting..."
      cleaner state conn
      release conn

    Left ex ->
      err (appLogger state) ("Cleaner: " <> pack (show ex))

  put (appLogger state) "Cleaner sleeping..."
  sleep (60 * 60) -- sleep for 1 hour

cleaner :: AppState -> Connection -> IO ()
cleaner state conn = do
  run (runWriteTransaction cleanOldSessions) conn >>= report state
  run (runWriteTransaction cleanOldNewUsers) conn >>= report state
  run (runWriteTransaction cleanOldLostPasswords) conn >>= report state

report :: AppState -> Either Error t -> IO ()
report state = \case
  Right _ ->
    pure ()
  Left ex ->
    err (appLogger state) ("Cleaner: " <> pack (show ex))
