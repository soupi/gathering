{- | Displaying events, creating new events and editing existing events

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Actions.Attending where

import Web.Gathering.Types
import Web.Gathering.Database
import Web.Gathering.Actions.Utils

import qualified Data.Text as T
import qualified Hasql.Session as Sql (run)
import Data.HVect (HVect(..), ListContains, findFirst)
import Data.Monoid

import Web.Spock

-- | Describe the action to do when a user wants to create a new event
--
--   Will present the event/new form and will take care of the validation,
--   will query the database for validation and will insert the new event
--
attendingAction :: (ListContains n User xs) => EventId -> Maybe Bool -> Action (HVect xs) ()
attendingAction eid mIsAttending = do
  user <- fmap findFirst getContext
  getResult <- runQuery $ Sql.run (runReadTransaction $ getEventById eid)
  case getResult of
    -- @TODO this is an internal error that we should take care of internally
    Left (T.pack . show -> e) -> do
      err e
      text e

    Right Nothing -> do
      text "Event not found."

    Right (Just event) -> do
      upsertResult <- runQuery $ Sql.run $
        case mIsAttending of
          Just isAttending ->
            runWriteTransaction $ upsertAttendant event (Attendant user isAttending isAttending)
          Nothing ->
            runWriteTransaction $ removeAttendant event user
      case upsertResult of
        -- @TODO this is an internal error that we should take care of internally
        Left (T.pack . show -> e) -> do
          err e
          text e
        Right _ ->
          redirect $ "/event/" <> T.pack (show eid)
