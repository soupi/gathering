{- | This module takes care of the authentication of a user

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Auth where

import Web.Gathering.Types
import Web.Gathering.Database
import Web.Gathering.HashPassword
import qualified Web.Gathering.Forms.Sign as FS

import Data.Monoid
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Hasql.Session as Sql (run)
import Data.HVect (HVect(..), ListContains, NotInList, findFirst)

import Web.Spock
import Web.Spock.Lucid
import Web.Spock.Digestive

import Lucid (p_)

-----------
-- Hooks --
-----------

-- Hooks are the context of the app and provides us
-- a type safe way to check we don't call functions we are not supposed to
-- call. For example, Only guests should be able to sign-in or sign-up

-- | A label for the guest only hook to mark that the user is not logged in
data IsGuest = IsGuest
  deriving (Show, Eq, Ord)

-- | A label for the admin hook to mark that the user is an administrator
data IsAdmin = IsAdmin
  deriving (Show, Eq, Ord)


-- | Verifies that the user is a guest to the site.
--   If they aren't it will redirect to root
guestOnlyHook :: Action (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook =
  maybeUser $ \mUser -> do
    oldCtx <- getContext
    case mUser of
      Nothing -> return (IsGuest :&: oldCtx)
      Just _ -> redirect "/"

-- | Checks that the user is logged in.
--   If they aren't it will display an error message
authHook :: Action (HVect xs) (HVect (User ': xs))
authHook =
  maybeUser $ \mUser -> do
    oldCtx <- getContext
    case mUser of
      Nothing ->
        redirect "/signin"
      Just val ->
        pure (val :&: oldCtx)

-- | Checks that the user is an admin.
--   If they aren't it will display an error message
adminHook :: ListContains n User xs => Action (HVect xs) (HVect (IsAdmin ': xs))
adminHook = do
  user <- fmap findFirst getContext
  oldCtx <- getContext
  if userIsAdmin user
    then pure (IsAdmin :&: oldCtx)
    else text "You don't have permissions to view this, sorry,"


-------------
-- Actions --
-------------

-- Actions describe the actions to perform when a user wants to sign-in/up/out


-- | Describe the action to do when a user wants to sign into the system:
--
--   Will present the sign-in form and will take care of the validation,
--   will query the database for validation, will insert the new session
--   and will write it in the session state on success
--
signInAction :: (ListContains n IsGuest xs, NotInList User xs ~ 'True) => Action (HVect xs) ()
signInAction = do
  -- Run the form
  form <- runForm "loginForm" FS.signinForm
  -- validate the form.
  -- Nothing means failure. will display the form view back to the user when validation fails.
  case form of
    (view, Nothing) ->
      lucid $ formView Nothing view
    -- If basic validation of fields succeeds, continue to check validation against db

    (view, Just FS.Signin{sinLogin, sinPassword}) -> do
      -- Query the db for a match for the login
      -- (will check if sinLogin match either the user name or email)
      maybeUserAndPass <- runQuery $ Sql.run (getUserLogin sinLogin sinLogin)
      case maybeUserAndPass of
        -- @TODO this is an internal error that we should take care of internally
        Left err -> do
          text $ T.pack (show err)

        Right Nothing ->
          lucid $ formView (pure $ p_ "Invalid user name/email.") view

        Right (Just (user, pass)) -> do
          if verifyPassword (T.encodeUtf8 sinPassword) pass
            then do -- success - create the session for the user
              makeSession (userId user) $
                text $ "Logged in as: " <> userName user
            else
              lucid $ formView (pure $ p_ "Invalid password.") view

  where
    -- | Display the form to the user
    formView mErr view = do
      maybe (pure ()) id mErr
      FS.signinFormView view

-- | Describe the action to do when a user wants to sign up for the system:
--
--   Will present the sign-up form and will take care of the validation,
--   will query the database for validation, will insert the new session
--   and will write it in the session state on success
--
signUpAction :: (ListContains n IsGuest xs, NotInList User xs ~ 'True) => Action (HVect xs) ()
signUpAction = do
  -- Run the form
  form <- runForm "registerForm" FS.signupForm
  -- validate the form.
  -- Nothing means failure. will display the form view back to the user when validation fails.
  case form of
    (view, Nothing) ->
      lucid $ formView Nothing view

    -- Case for bots
    (_, Just (FS.Signup { supUsername, supSpamHoneyPot }))
      | not (T.null supSpamHoneyPot) -> do
        text $ "Success! Now logged in as: " <> supUsername -- gotcha!

    -- Case for humans
    (view, Just (FS.Signup uname umail pass passConfirm notify _)) -> do
      -- Query the db for a match for the login
      -- will check if the users' requested name or email already exists
      maybeUserAndPass <- runQuery $ Sql.run (getUserLogin uname umail)
      case maybeUserAndPass of
        -- @TODO this is an internal error that we should take care of internally
        Left err -> do
          text $ T.pack (show err)

        Right (Just _) -> do
            lucid $ formView (pure $ p_ "Username or email already exists.") view

        Right Nothing
          | pass /= passConfirm ->
            lucid $ formView (pure $ p_ "Passwords do not match.") view

        -- User does not exists and passwords match. try to create a new user
        Right Nothing -> do
          hashedPass <- liftIO $ makePassword pass
          mNewUser <-
            runQuery $ Sql.run $ newUser (User 0 uname umail False notify) hashedPass
          -- @TODO this is an internal error that we should take care of internally
          case mNewUser of
            Left err ->
              text $ T.pack (show err)

            Right nUser -> do
              makeSession (userId nUser) $
                text $ "Success! Now logged in as: " <> userName nUser
  where
    formView mErr view = do
      maybe (pure ()) id mErr
      FS.signupFormView view


signOutAction :: (ListContains n User xs) => Action (HVect xs) ()
signOutAction = do
  sess <- readSession
  case sess of
    EmptySession ->
      text "Not logged in."
    SessionId uid -> do
      writeSession EmptySession
      void $ runQuery $ Sql.run $ killSession uid -- maybe log this?
      text "Logged out."



-----------
-- Utils --
-----------

-- | Insert the user session to the database and write in to the app state
--   So the user can continue using the website while being logged in
makeSession :: (ListContains n IsGuest xs, NotInList User xs ~ 'True)
            => UserId -> Action (HVect xs) () -> Action (HVect xs) ()
makeSession uid act = do
  sessRes <- runQuery $ Sql.run $ upsertUserSession uid
  case sessRes of
    Left err -> do
      text $ T.pack (show err)
    Right _ -> do
      writeSession (SessionId uid)
      act

-- | Get the user from session if logged in and run the action by passing it the result
maybeUser :: (Maybe User -> Action ctx a) -> Action ctx a
maybeUser action = do
  sess <- readSession
  case sess of
    EmptySession ->
      action Nothing

    SessionId uid -> do
      emUser <- runQuery $ Sql.run (getUserBySession uid)
      case emUser of
        -- @TODO this is an internal error that we should take care of internally
        Left err ->
          text $ T.pack $ show err

        Right mu ->
          action mu

