{- | The entry point to the application

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Run where

import Web.Gathering.Types
import Web.Gathering.Config
import Web.Gathering.Database
import Web.Gathering.HashPassword
import qualified Web.Gathering.Forms.Sign as FS

import Data.HVect
import Data.Monoid
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
--import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BSC

import Hasql.Connection (Connection)
import qualified Hasql.Session as Sql (run)

import Web.Spock
import Web.Spock.Lucid
import Web.Spock.Config
import Web.Spock.Digestive

import Lucid


run :: IO ()
run = do
  (config, cmd) <- parseArgs
  print (config, cmd)
  let connstr = BSC.pack $ cfgDbConnStr config
  spockCfg <- defaultSpockCfg EmptySession (PCConn $ hasqlPool connstr) (AppState config)
  case cmd of
    HTTP port ->
      runSpock port (spock spockCfg app)
    HTTPS tls -> do
      runHttps spockCfg tls
    Both port tls -> do
      void $ forkIO $ runSpock port (spock spockCfg app)
      runHttps spockCfg tls

runHttps :: SpockCfg Connection MySession AppState -> TLSConfig -> IO ()
runHttps spockCfg tls = do
  spockApp <- spockAsApp (spock spockCfg app)
  runTLS
    (tlsSettings (tlsKey tls) (tlsCert tls))
    (setPort (tlsPort tls) defaultSettings)
    spockApp

app :: App ()
app = prehook baseHook $ do
  prehook guestOnlyHook $ do

    getpost ("signup") signupAction
    getpost ("register") (redirect "signup")

    getpost ("signin") signinAction
    getpost ("login") (redirect "signin")

  get root $ maybeUser $ \case
    Just (User { userName }) ->
      text $ "Hello " <> userName <> "!"
    Nothing ->
      text "Hello World!"

  get ("hello" <//> var) $ \name -> do
    AppState cfg <- getState
    text ("Hello " <> name <> ", welcome to " <> T.pack (show $ cfgTitle cfg))

  prehook authHook $ do
    get ("settings") $ do
      (user :: User) <- fmap findFirst getContext
      text ("Hello " <> userName user)

    get ("signout") $ do
      sess <- readSession
      case sess of
        EmptySession ->
          text "Not logged in."
        SessionId uid -> do
          writeSession EmptySession
          void $ runQuery $ Sql.run $ killSession uid -- maybe log this?
          text "Logged out."

    get ("logout") $ redirect "signout"

signinAction :: (ListContains n IsGuest xs, NotInList User xs ~ 'True) => Action (HVect xs) ()
signinAction = do
  form <- runForm "loginForm" FS.signinForm
  let
    formView mErr view = do
      maybe (pure ()) id mErr
      FS.signinFormView view
  case form of
    (view, Nothing) ->
      lucid $ formView Nothing view
    (view, Just FS.Signin{sinLogin, sinPassword}) -> do
      signInRes <- runQuery $ Sql.run (getUserLogin sinLogin sinLogin)
      case signInRes of
        Left err -> do
          text $ T.pack (show err)
        Right Nothing ->
          lucid $ formView (pure $ p_ "Invalid user name/email.") view
        Right (Just (user, pass)) -> do
          if verifyPassword (T.encodeUtf8 sinPassword) pass
            then do
              makeSession (userId user) $
                text $ "Logged in as: " <> userName user
            else
              lucid $ formView (pure $ p_ "Invalid password.") view

signupAction :: (ListContains n IsGuest xs, NotInList User xs ~ 'True) => Action (HVect xs) ()
signupAction = do
  form <- runForm "registerForm" FS.signupForm
  let
    formView mErr view = do
      maybe (pure ()) id mErr
      FS.signupFormView view
  case form of
    (view, Nothing) ->
      lucid $ formView Nothing view
    (view, Just (FS.Signup uname umail pass passConfirm notify)) -> do
      signinRes <-
        runQuery $ Sql.run (getUserLogin uname umail)
      case signinRes of
        Left err -> do
          text $ T.pack (show err)
        Right (Just _) -> do
            lucid $ formView (pure $ p_ "Username or email already exists.") view
        Right _
          | pass /= passConfirm ->
            lucid $ formView (pure $ p_ "Passwords do not match.") view
        Right _ -> do
          hashedPass <- liftIO $ makePassword pass
          mNewUser <-
            runQuery $ Sql.run $ newUser (User 0 uname umail False notify) hashedPass
          case mNewUser of
            Left err ->
              text $ T.pack (show err)
            Right nUser -> do
              makeSession (userId nUser) $
                text $ "Success! Now logged in as: " <> userName nUser

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

-----------
-- Hooks --
-----------

baseHook :: Action () (HVect '[])
baseHook = return HNil

authHook :: Action (HVect xs) (HVect (User ': xs))
authHook =
  maybeUser $ \mUser -> do
    oldCtx <- getContext
    case mUser of
      Nothing ->
        text "Unknown user. Sign-in first!"
      Just val ->
        pure (val :&: oldCtx)

data IsAdmin = IsAdmin
  deriving (Show, Eq, Ord)

adminHook :: ListContains n (UserId, User) xs => Action (HVect xs) (HVect (IsAdmin ': xs))
adminHook = do
  (_ :: UserId, user) <- fmap findFirst getContext
  oldCtx <- getContext
  if userIsAdmin user
    then pure (IsAdmin :&: oldCtx)
    else text "You don't have enough rights, sorry"

data IsGuest = IsGuest
  deriving (Show, Eq, Ord)

guestOnlyHook :: Action (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook =
  maybeUser $ \mUser -> do
    oldCtx <- getContext
    case mUser of
      Nothing -> return (IsGuest :&: oldCtx)
      Just _ -> redirect "/"

maybeUser :: (Maybe User -> Action ctx a) -> Action ctx a
maybeUser action = do
  sess <- readSession
  case sess of
    EmptySession ->
      action Nothing
    SessionId uid -> do
      emUser <-
        runQuery $ Sql.run (getUserBySession uid)
      case emUser of
        Left err ->
          text $ T.pack $ show err
        Right mu ->
          action mu
