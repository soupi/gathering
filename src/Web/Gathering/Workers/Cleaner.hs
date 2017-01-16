{- | A worker that runs in the background and cleans old data from tables

-}


module Web.Gathering.Workers.Cleaner where

import Data.Text (pack)
import Turtle
import Hasql.Session
import Hasql.Connection

import Web.Gathering.Database

cleanerWorker :: Connection -> IO ()
cleanerWorker conn = forever $ do
  cleaner conn
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
