{- | Displaying events, creating new events and editing existing events

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Actions.Events where

import Web.Gathering.Utils
import Web.Gathering.Types
import Web.Gathering.Config
import Web.Gathering.Database
import Web.Gathering.Actions.Utils
import Web.Gathering.Actions.Auth (IsAdmin)
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
displayEvents getEventsQuery mUser = do
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
      lucid $ Html.renderEvents title mUser eventsAA

-- | Describe the action to do when a user wants to create a new event
--
--   Will present the event/new form and will take care of the validation,
--   will query the database for validation and will insert the new event
--
newEventAction :: (ListContains n User xs, ListContains m IsAdmin xs) => Action (HVect xs) ()
newEventAction = do
  -- Run the form
  form <- runForm path (editEventForm Nothing)
  -- validate the form.
  -- Nothing means failure. will display the form view back to the user when validation fails.
  case form of
    (view, Nothing) ->
      lucid $ formView Nothing view
    -- If basic validation of fields succeeds, continue to check validation against db

    (_, Just (EditEvent name desc loc mWhen mDur))
      | Just when <- parseDateTime mWhen
      , Just dur  <- parseDiffTime mDur
      -> do
        result <- runQuery $ Sql.run (newEvent $ Event 0 name desc loc when dur) -- newEvent doesn't care about event_id
        case result of
          -- @TODO this is an internal error that we should take care of internally
          Left err -> do
            text $ T.pack (show err)

          Right eid ->
            redirect ("/event/" <> T.pack (show eid))

    (_, Just eEvent) ->
      reportEventParsingError eEvent

  where
    formView = formViewer $ editEventFormView path "Create"
    path = "/event/new"

-- | Describe the action to do when a user wants to edit an existing event
--
--   Will present the edit event form and will take care of the validation,
--   will query the database for validation and will update the event
--
editEventAction :: (ListContains n User xs, ListContains m IsAdmin xs) => EventId -> Action (HVect xs) ()
editEventAction eid = do
  mEditedEvent <- runQuery $ Sql.run (getEventById eid)
  case mEditedEvent of
    -- @TODO this is an internal error that we should take care of internally
    Left err ->
      text $ T.pack (show err)

    Right Nothing ->
      text "Event does not exist"

    Right (Just editedEvent) -> do

      -- Run the form
      form <- runForm path (editEventForm $ Just $ eventToEditEvent editedEvent)
      -- validate the form.
      -- Nothing means failure. will display the form view back to the user when validation fails.
      case form of
        (view, Nothing) ->
          lucid $ formView Nothing view
        -- If basic validation of fields succeeds, continue to check validation against db

        (_, Just (EditEvent name desc loc mWhen mDur))
          | Just when <- parseDateTime mWhen
          , Just dur  <- parseDiffTime mDur
          -> do
            result <- runQuery $ Sql.run (updateEvent $ Event eid name desc loc when dur)
            case result of
              -- @TODO this is an internal error that we should take care of internally
              Left err -> do
                text $ T.pack (show err)

              Right _ ->
                redirect ("/event/" <> T.pack (show eid))

        (_, Just eEvent) ->
          reportEventParsingError eEvent

  where
    formView = formViewer $ editEventFormView path "Update"
    path = "/event/" <> T.pack (show eid) <> "/edit"


-- | Describe the action to do when a user wants to delete an existing event
--
removeEventAction :: (ListContains n User xs, ListContains m IsAdmin xs) => EventId -> Action (HVect xs) ()
removeEventAction eid = do
  mEvent <- runQuery $ Sql.run (getEventById eid)
  case mEvent of
    -- @TODO this is an internal error that we should take care of internally
    Left err ->
      text $ T.pack (show err)

    Right Nothing ->
      text "Event does not exist"

    Right (Just event) -> do
      result <- runQuery $ Sql.run (removeEvent event)
      case result of
        -- @TODO this is an internal error that we should take care of internally
        Left err -> do
          text $ T.pack (show err)

        Right _ ->
          redirect "/"


reportEventParsingError :: EditEvent -> Action (HVect xs) ()
reportEventParsingError eEvent =
  text $ T.unlines
    [ "Internal error. Please report the following:"
    , "duration: '" <> eEventDuration eEvent <> "': " <> (T.pack . show . parseDiffTime . eEventDuration $ eEvent)
    , "datetime: '" <> eEventDateTime eEvent <> "': " <>
        (T.pack . show $ (parseDateTime (eEventDateTime eEvent) :: Maybe UTCTime))
    ]
