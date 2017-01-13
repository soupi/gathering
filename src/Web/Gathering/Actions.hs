{- | Various spock actions

For now displaying events and creating new events

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Actions where

import Web.Gathering.Auth (IsAdmin)
import Web.Gathering.Types
import Web.Gathering.Config
import Web.Gathering.Database
import Web.Gathering.Forms.EditEvent
import qualified Web.Gathering.Html as Html

import qualified Data.Text as T
import qualified Hasql.Session as Sql (run, Session)
import Data.HVect (HVect(..), ListContains)
import Data.Monoid
import Data.Time

import Web.Spock
import Web.Spock.Lucid
import Web.Spock.Digestive

-- | Display events. allows the caller to specify which events to get from the database and in which order.
displayEvents :: (Sql.Session [Event]) -> Maybe User -> Action (HVect xs) ()
displayEvents getEventsQuery _ = do
  mEventsAndAtts <- runQuery $ Sql.run $ do
    events <- getEventsQuery
    mapM (\e -> (e,) <$> getAttendantsForEvent e) events
  case mEventsAndAtts of
    -- @TODO this is an internal error that we should take care of internally
    Left err -> do
      text $ T.pack (show err)

    -- case where there are no events to display
    Right eventsAA | null eventsAA -> do
      title <- cfgTitle . appConfig <$> getState
      lucid $ Html.noEvents title

    Right eventsAA -> do
      title <- cfgTitle . appConfig <$> getState
      lucid $ Html.renderEvents title eventsAA

-- | Describe the action to do when a user wants to create a new event
--
--   Will present the new-event form and will take care of the validation,
--   will query the database for validation and will insert the new event
--
newEventAction :: (ListContains n User xs, ListContains m IsAdmin xs) => Action (HVect xs) ()
newEventAction = do
  -- Run the form
  form <- runForm "new-event" editEventForm
  -- validate the form.
  -- Nothing means failure. will display the form view back to the user when validation fails.
  case form of
    (view, Nothing) ->
      lucid $ formView Nothing view
    -- If basic validation of fields succeeds, continue to check validation against db

    (_, Just (EditEvent name desc loc mWhen mDur))
      | Just when <- parseDateTime mWhen
      , Just dur  <- toDuration mDur
      -> do
        result <- runQuery $ Sql.run (newEvent $ Event 0 name desc loc when dur)
        case result of
          -- @TODO this is an internal error that we should take care of internally
          Left err -> do
            text $ T.pack (show err)

          Right _ ->
            text "Event submitted."

    (_, Just eEvent) ->
        text $ T.unlines
        [ "Internal error. Please report the following:"
        , "duration: '" <> eEventDuration eEvent <> "': " <> (T.pack . show . toDuration . eEventDuration $ eEvent)
        , "datetime: '" <> eEventDateTime eEvent <> "': " <>
            (T.pack . show $
              (parseTimeM True defaultTimeLocale "%Z, %F (%Z)" $ T.unpack (eEventDateTime eEvent) :: Maybe UTCTime))
        ]

  where
    -- | Display the form to the user
    formView mErr view = do
      maybe (pure ()) id mErr
      editEventFormView "new-event" view
