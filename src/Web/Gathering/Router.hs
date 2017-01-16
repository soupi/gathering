{- | The router of the application.

It will direct which action should run according to the route and app state.

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

import Data.HVect
import Data.Monoid
import Data.Int (Int32)
import Data.Text (Text)
import Data.Maybe (maybeToList)
import Network.Wai.Middleware.Static (staticPolicy, addBase)

import Web.Spock


-------------
-- Routing --
-------------

-- | This is the router of the app
--   It will direct which action should run
--   according to the route and app state
--
appRouter :: App ()
appRouter = prehook baseHook $ do

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

  -- authentication

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

  -- signed-in users zone

  prehook authHook $ do
    -- temp
    get "settings" $ do
      (user :: User) <- fmap findFirst getContext
      text ("Hello " <> userName user)

    get "signout" $
      signOutAction

    get "logout" $
      redirect "signout"

    get ("event" <//> var <//> "attending") $ \(eid :: EventId) ->
      attendingAction eid (Just True)

    get ("event" <//> var <//> "not-attending") $ \(eid :: EventId) ->
      attendingAction eid (Just False)

    get ("event" <//> var <//> "remove-attending") $ \(eid :: EventId) ->
      attendingAction eid Nothing

    -- administrators zone

    prehook adminHook $ do

      getpost ("event" <//> "new") $
        newEventAction

      getpost ("event" <//> var <//> "edit") $ \(eid :: EventId) ->
        editEventAction eid

      get ("event" <//> var <//> "delete") $ \(eid :: EventId) ->
        removeEventAction eid

-----------
-- Hooks --
-----------

-- Hooks are the context of the app and provides us
-- a type safe way to check we don't call functions we are not supposed to
-- call. For example, Only guests should be able to sign-in or sign-up

baseHook :: Action () (HVect '[])
baseHook = return HNil
