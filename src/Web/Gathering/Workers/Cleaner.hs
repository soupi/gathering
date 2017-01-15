{- | A worker that runs in the background and cleans old data from tables

-}


module Web.Gathering.Workers.Cleaner where

import Data.Text (pack)
import Turtle
import Hasql.Session
import Hasql.Connection

import Web.Gathering.Database

cleaner :: Connection -> IO ()
cleaner conn = forever $ do
  result <- run cleanOldSessions conn
  case result of
    Right _ -> pure ()
    Left ex ->
      err ("Cleaner: " <> pack (show ex))
  sleep (60 * 60 * 24) -- sleep for 24 hours

