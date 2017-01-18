{- | A worker that runs in the background and cleans old data from tables

-}


module Web.Gathering.Workers.Cleaner where

import Data.Text (pack)
import Turtle (forever, (<>), sleep)
import Hasql.Session
import Hasql.Connection

import Web.Gathering.Database
import Web.Gathering.Types
import Web.Gathering.Config
import Web.Gathering.Utils

cleanerWorker :: AppState -> IO ()
cleanerWorker state = forever $ do
  mConn <- acquire (cfgDbConnStr $ appConfig state)

  case mConn of
    Right conn -> do
      putStrTime "Cleaner starting..."
      cleaner conn
      release conn

    Left ex ->
      errTime ("Cleaner: " <> pack (show ex))

  putStrTime "Cleaner sleeping..."
  sleep (60 * 60) -- sleep for 1 hour

cleaner :: Connection -> IO ()
cleaner conn = do
  run (runWriteTransaction cleanOldSessions) conn >>= report
  run (runWriteTransaction cleanOldNewUsers) conn >>= report

report :: Either Error t -> IO ()
report = \case
  Right _ ->
    pure ()
  Left ex ->
    errTime ("Cleaner: " <> pack (show ex))
