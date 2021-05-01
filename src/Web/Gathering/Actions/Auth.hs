{- | This module takes care of the authentication of a user

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Actions.Auth where

import Web.Gathering.Types
import Web.Gathering.Database
import Web.Gathering.HashPassword
import Web.Gathering.Forms.Utils
import Web.Gathering.Actions.Utils
import Web.Gathering.Workers.SendEmails
import qualified Web.Gathering.Forms.Sign as FS
import qualified Web.Gathering.Forms.Settings as FS

import Data.Int (Int32)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.HVect (HVect(..), ListContains, NotInList, findFirst)

import Web.Spock
import Web.Spock.Digestive

import Lucid (p_, class_, toHtml)

-----------
-- Hooks --
-----------

-- Hooks are the context of the app and provides us
-- a type safe way to check we don't call functions we are not supposed to
-- call. For example, Only guests should be able to sign-in or sign-up
--
-- for more info consult this guide: https://www.spock.li/2015/04/19/type-safe_routing.html


-- | A label for the guest only hook to mark that the user is not logged in
data IsGuest = IsGuest
  deriving (Show, Eq, Ord)

-- | A label for the admin hook to mark that the user is an administrator
data IsAdmin = IsAdmin
  deriving (Show, Eq, Ord)


-- | The base hook
baseHook :: Action () (HVect '[])
baseHook = return HNil

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
  ac <- appConfig <$> getState
  let
    -- | Display the form to the user
    formView mErr view = do
      form <- secureForm "signin" FS.signinFormView view
      formViewer ac "Sign-in" form mErr

  -- Run the form
  form <- runForm "" FS.signinForm
  -- validate the form.
  -- Nothing means failure. will display the form view back to the user when validation fails.
  case form of
    (view, Nothing) ->
      formView Nothing view
    -- If basic validation of fields succeeds, continue to check validation against db

    (view, Just FS.Signin{sinLogin, sinPassword}) -> do
      -- Query the db for a match for the login
      -- (will check if sinLogin match either the user name or email)
      maybeUserAndPass <- readQuery (getUserLogin sinLogin sinLogin)
      case maybeUserAndPass of
        -- @TODO this is an internal error that we should take care of internally
        Left (T.pack . show -> e) -> do
          err e
          text e

        Right Nothing ->
          formView (pure $ p_ [ class_ "error" ] "Invalid user name/email.") view

        Right (Just (user, pass)) -> do
          if verifyPassword (T.encodeUtf8 sinPassword) pass
            then do -- success - create the session for the user
              makeSession (userId user) $
                redirect "/"
            else
              formView (pure $ p_ [ class_ "error" ] "Invalid password.") view

-- | Describe the action to do when a user wants to sign up for the system:
--
--   Will present the sign-up form, will take care of the validation and
--   will query the database for validation.
--
--   to the user's email and will write the user details to the new_users table
--   On a successful sign-up it will send the verification link
--
signUpAction :: (ListContains n IsGuest xs, NotInList User xs ~ 'True) => Action (HVect xs) ()
signUpAction = do
  ac <- appConfig <$> getState
  let
    -- | Display the form to the user
    formView mErr view = do
      form <- secureForm "signup" FS.signupFormView view
      formViewer ac "Sign-up" form mErr

  -- Run the form
  form <- runForm "" FS.signupForm
  -- validate the form.
  -- Nothing means failure. will display the form view back to the user when validation fails.
  case form of
    (view, Nothing) ->
      formView Nothing view

    -- Case for bots
    (_, Just (FS.Signup { supEmail, supSpamHoneyPot }))
      | not (T.null supSpamHoneyPot) -> do
        warn ("HoneyPot detected: " <> supEmail)
        text $ "Verification email sent to " <> supEmail <> ". Note that it will expire in two days."
          <> "\n\nPlease give it a few minutes and check your spam folder as well."

    -- Case for humans
    (view, Just (FS.Signup uname umail pass passConfirm notify _)) -> do
      -- Query the db for a match for the login
      -- will check if the users' requested name or email already exists
      maybeUserAndPass <- readQuery (getUserLogin uname umail)
      case maybeUserAndPass of
        -- @TODO this is an internal error that we should take care of internally
        Left (T.pack . show -> e) -> do
          err e
          text e

        Right (Just _) -> do
            formView (pure $ p_ [ class_ "error" ] "Username or email already exists.") view

        Right Nothing
          | pass /= passConfirm ->
            formView (pure $ p_ [ class_ "error" ] "Passwords do not match.") view

        -- User does not exists and passwords match. try to create a new user
        Right Nothing -> do
          hashedPass <- liftIO $ makePassword pass
          mNewUser <-
            writeQuery $ newUser (User 0 uname umail False notify "") hashedPass
          -- @TODO this is an internal error that we should take care of internally
          case mNewUser of
            Left (T.pack . show -> e) -> do
              err e
              text e

            Right (Left e) ->
              text e

            Right (Right nUser) -> do
              state  <- getState
              result <- liftIO $ (pure <$> notifyVerification state nUser)
                `catch` \ex -> pure $ Left (T.pack $ show (ex :: SomeException))
              case result of
                Right () ->
                  text $ "Verification email sent to " <> userEmail nUser <> ". Note that it will expire in two days."
                    <> "\n\nPlease give it a few minutes and check your spam folder as well."

                Left e -> do
                  void . writeQuery $ removeNewUser nUser
                  err e
                  -- @TODO internal err
                  text $ "Failed to send email. Please verify your mail is valid and try again later.\n\n" <> e


-- | Sign out a signed-in user
signOutAction :: (ListContains n User xs) => Action (HVect xs) ()
signOutAction = do
  sess <- readSession
  case sess of
    EmptySession ->
      text "Not logged in."
    SessionId uid -> do
      writeSession EmptySession
      void . writeQuery $ killSession uid -- maybe log this?
      redirect "/"

-- | Verify a new user and set user session on success
verificationAction :: (ListContains n IsGuest xs, NotInList User xs ~ 'True) => Int32 -> T.Text -> Action (HVect xs) ()
verificationAction key email = do
  result <- writeQuery $ verifyNewUser key email
  case result of
    -- @TODO internal err
    Left (T.pack . show -> e) -> do
      err e
      text e

    Right (Left e) ->
      text e

    Right (Right user) -> do
      void . writeQuery $ removeNewUser user
      makeSession (userId user) $
        redirect "/"

-- | Unsubscribe a user on request
unsubscribeAction :: T.Text -> T.Text -> Action (HVect xs) ()
unsubscribeAction email key = do

  mUser <- readQuery $ getUserLogin "" email
  case mUser of
    -- @TODO internal err
    Left (T.pack . show -> e) -> do
      err e
      text e

    Right Nothing ->
      text "Unknown email."

    Right (Just (user,_)) -> do
      if hashMD5 (userHash user) (userEmail user) == key
        then do
          void . writeQuery $ updateUser (user { userWantsUpdates = False })
          text "You will not receive further mails until you change your settings."
        else do
          text "Email and key does not match. Please log in and change your settings to unsubscribe."



-- | Give the user the ability to change their settings
-- (only for receiving notifications for now)
settingsAction :: (ListContains n User xs) => Action (HVect xs) ()
settingsAction = do
  user <- fmap findFirst getContext
  ac <- appConfig <$> getState
  let
    -- | Display the form to the user
    formView mErr view = do
      form <- secureForm "settings" FS.settingsFormView view
      formViewer ac "Settings" form mErr

  -- Run the form
  form <- runForm "" (FS.settingsForm user)
  -- validate the form.
  -- Nothing means failure. will display the form view back to the user when validation fails.
  case form of
    (view, Nothing) ->
      formView Nothing view

    (_, Just (FS.Settings wantsUpdates )) -> do
      result <- writeQuery $ updateUser (user { userWantsUpdates = wantsUpdates })
      case result of
        -- Handling the left `QueryError` case for `writeQuery`
        Left (T.pack . show -> e) -> do
          err e
          text e

        -- Handling the internal error that `updateUser` can now return
        Right (Left e) -> do
          err e
          text e

        -- Success
        Right _ -> do
          redirect "/"

-- | Handle a reset password request action
-- | Will send the user's email a link to a reset password form
requestResetAction :: (ListContains n IsGuest xs, NotInList User xs ~ 'True) => Action (HVect xs) ()
requestResetAction = do
  ac <- appConfig <$> getState
  let
    -- | Display the form to the user
    formView mErr view = do
      form <- secureForm "lost-password" FS.requestResetFormView view
      formViewer ac "lost-password" form mErr

  form <- runForm "" FS.requestResetForm

  case form of
    (view, Nothing) ->
      formView Nothing view

    (view, Just email) -> do
      mUserHash <- writeQuery $ requestResetPassword email
      case mUserHash of
        -- @TODO  handle internally?
        Left (T.pack . show -> e) -> do
          text e

        Right (Left e) -> do
          formView (pure . p_ [ class_ "error" ] $ toHtml e) view

        Right (Right (u, h)) -> do
         state  <- getState
         result <- liftIO $ (pure <$> notifyResetPassword state u h)
           `catch` \ex -> pure $ Left (T.pack $ show (ex :: SomeException))

         case result of
           Left e -> do
             void . writeQuery $ removeResetPassword (userId u)
             err e
             text $ "Failed to send email. Please verify your mail is valid and try again later.\n\n" <> e

           Right () ->
             text $ "Reset password request sent to " <> email <> ". Note that it will expire in one day."
               <> "\n\nPlease give it a few minutes and check your spam folder as well."


-- | Will let the user fill a new password
resetPasswordAction :: (ListContains n IsGuest xs, NotInList User xs ~ 'True) => T.Text -> T.Text -> Action (HVect xs) ()
resetPasswordAction hash email = do
  ac <- appConfig <$> getState
  let
    -- | Display the form to the user
    formView mErr view = do
      form <- secureForm ("/reset-password/" <> hash <> "/" <> email) FS.resetPasswordFormView view
      formViewer ac "reset-password" form mErr

  form <- runForm "" FS.resetPasswordForm

  case form of
    (view, Nothing) ->
      formView Nothing view

    (view, Just (FS.ResetPassword pass confirmPass))
      | pass /= confirmPass ->
          formView (pure $ p_ [ class_ "error" ] "Passwords do not match.") view

    (view, Just (FS.ResetPassword pass _)) -> do
      hashedPass <- liftIO $ makePassword pass
      result <- writeQuery $ verifyResetPassword hash email hashedPass
      case result of
        Left (T.pack . show -> e) -> do
          err e
          text e

        Right (Left e) ->
          formView (pure . p_ [ class_ "error" ] $ toHtml e) view

        Right (Right user) -> do
          void . writeQuery $ removeResetPassword (userId user)
          makeSession (userId user) $
            redirect "/"



-----------
-- Utils --
-----------

-- | Insert the user session to the database and write it in the app state
--   So the user can continue using the website while being signed-in
makeSession :: (ListContains n IsGuest xs, NotInList User xs ~ 'True)
            => UserId -> Action (HVect xs) () -> Action (HVect xs) ()
makeSession uid act = do
  sessionRegenerateId
  sessRes <- writeQuery $ upsertUserSession uid
  case sessRes of
    Left (T.pack . show -> e) -> do
      err e
      text e
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
      emUser <- readQuery (getUserBySession uid)
      case emUser of
        -- @TODO this is an internal error that we should take care of internally
        Left (T.pack . show -> e) -> do
          err e
          text e

        Right mu ->
          action mu

