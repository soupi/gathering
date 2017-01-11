{- | The router of the application.

It will direct which action should run according to the route and app state.

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Router where

import Web.Gathering.Types
import Web.Gathering.Config
import Web.Gathering.Auth

import Data.HVect
import Data.Monoid
import qualified Data.Text as T
--import qualified Network.HTTP.Types.Status as Http

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

  -- regular stuff

  get root $ maybeUser $ \case
    -- temp
    Just (User { userName }) ->
      text $ "Hello " <> userName <> "!"
    Nothing ->
      text "Hello World!"

  get ("hello" <//> var) $ \name -> do
    -- temp
    AppState cfg <- getState
    text ("Hello " <> name <> ", welcome to " <> T.pack (show $ cfgTitle cfg))

  -- authentication

  prehook guestOnlyHook $ do

    getpost ("signup") signUpAction
    getpost ("register") (redirect "signup")

    getpost ("signin") signInAction
    getpost ("login") (redirect "signin")

  -- user relevant stuff

  prehook authHook $ do
    -- temp
    get ("settings") $ do
      (user :: User) <- fmap findFirst getContext
      text ("Hello " <> userName user)

    get ("signout") $
      signOutAction

    get ("logout") $
      redirect "signout"


-----------
-- Hooks --
-----------

-- Hooks are the context of the app and provides us
-- a type safe way to check we don't call functions we are not supposed to
-- call. For example, Only guests should be able to sign-in or sign-up

baseHook :: Action () (HVect '[])
baseHook = return HNil
