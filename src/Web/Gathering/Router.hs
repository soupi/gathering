{- | The router of the application

The router directs http requests to the relevant handler/action to be taken

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Router where

import Web.Gathering.Types
import Web.Gathering.Actions.Auth
import Web.Gathering.Actions.Events
import Web.Gathering.Actions.Attending
import Web.Gathering.Database

import Data.Int (Int32)
import Data.Text (Text)
import Data.Maybe (maybeToList)
import Network.Wai.Middleware.Static (staticPolicy, addBase)

import Web.Spock


-------------
-- Routing --
-------------

-- | This is the router of the app
--
-- It uses hooks to to separate handlers between
-- different authentication and unauthenticated users
--
-- The hooks are defined in Web.Gathering.Actions.Auth
--
appRouter :: App ()
appRouter = prehook baseHook $ do

  -- serve static content like css and js that can be found in the static folder

  middleware (staticPolicy (addBase "static"))

  -- display events

  get root $ maybeUser $
    displayEvents (take 5 <$> getFutureEvents)

  get "events" $ maybeUser $
    displayEvents getFutureEvents

  get ("events" <//> "past") $ maybeUser $
    displayEvents getPastEvents

  get ("event" <//> var) $ \(eid :: EventId) ->
    maybeUser $ displayEvents (maybeToList <$> getEventById eid)

  get ("unsubscribe" <//> var <//> var) $ \(email :: Text) (key :: Text) ->
    unsubscribeAction email key

  -- authenticate guests

  prehook guestOnlyHook $ do

    getpost "signup"
      signUpAction
    getpost "register" $
      redirect "signup"

    getpost "signin"
      signInAction
    getpost "login" $
      redirect "signin"

    get ("verify-user" <//> var <//> var) $ \(key :: Int32) (email :: Text) ->
      verificationAction key email

    getpost ("lost-password") $
      requestResetAction

    getpost ("reset-password" <//> var <//> var) $ \(hash :: Text) (email :: Text) ->
      resetPasswordAction hash email

  -- signed-in users zone

  prehook authHook $ do
    getpost "settings" $ do
      settingsAction

    get "signout" $
      signOutAction

    get "logout" $
      redirect "signout"

    post ("event" <//> var <//> "attending") $ \(eid :: EventId) ->
      attendingAction eid (Just True)

    post ("event" <//> var <//> "not-attending") $ \(eid :: EventId) ->
      attendingAction eid (Just False)

    post ("event" <//> var <//> "remove-attending") $ \(eid :: EventId) ->
      attendingAction eid Nothing

    -- administrators zone

    prehook adminHook $ do

      getpost ("event" <//> "new") $
        newEventAction

      getpost ("event" <//> var <//> "edit") $ \(eid :: EventId) ->
        editEventAction eid

      getpost ("event" <//> var <//> "delete") $ \(eid :: EventId) ->
        deleteEventAction eid

