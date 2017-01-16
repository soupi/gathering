{- | A worker that runs in the background and cleans old data from tables

-}


module Web.Gathering.Workers.Cleaner where

import Data.Text (pack)
import Turtle
import Hasql.Session
import Hasql.Connection

import Web.Gathering.Database
import Web.Gathering.Types
import Web.Gathering.Config

cleanerWorker :: AppState -> IO ()
cleanerWorker state = forever $ do
  mConn <- acquire (cfgDbConnStr $ appConfig state)

  case mConn of
    Right conn -> do
      putStrLn "Cleaner starting..."
      cleaner conn
      release conn

    Left ex ->
      err ("Cleaner: " <> pack (show ex))

  putStrLn "Cleaner sleeping..."
  sleep (60 * 60) -- sleep for 1 hour

cleaner :: Connection -> IO ()
cleaner conn = do
  run cleanOldSessions conn >>= report
  run cleanOldNewUsers conn >>= report

report :: Either Error t -> IO ()
report = \case
  Right _ ->
    pure ()
  Left ex ->
    err ("Cleaner: " <> pack (show ex))
