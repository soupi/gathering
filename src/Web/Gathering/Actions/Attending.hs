{- | Will handle user attendance

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Actions.Attending where

import Web.Gathering.Types
import Web.Gathering.Database
import Web.Gathering.Actions.Utils

import qualified Data.Text as T
import Data.HVect (HVect(..), ListContains, findFirst)

import Web.Spock

-- | Handle the user action to attend or unattend an event
--
attendingAction :: (ListContains n User xs) => EventId -> Maybe Bool -> Action (HVect xs) ()
attendingAction eid mIsAttending = do
  user <- fmap findFirst getContext
  getResult <- readQuery (getEventById eid)
  case getResult of
    -- @TODO this is an internal error that we should take care of internally
    Left (T.pack . show -> e) -> do
      err e
      text e

    Right Nothing -> do
      text "Event not found."

    Right (Just event) -> do
      upsertResult <- writeQuery $
        case mIsAttending of
          Just isAttending ->
            upsertAttendant event (Attendant user isAttending isAttending)
          Nothing ->
            removeAttendant event user
      case upsertResult of
        -- @TODO this is an internal error that we should take care of internally
        Left (T.pack . show -> e) -> do
          err e
          text e
        Right _ ->
          redirect $ "/event/" <> T.pack (show eid)
