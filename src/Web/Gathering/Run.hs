-- | Run app according to configuration

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Run where

import Web.Gathering.Types
import Web.Gathering.Config
import Web.Gathering.Model
import Web.Gathering.Database
import qualified Web.Gathering.Forms.Sign as FS

import Data.HVect
import Data.Monoid
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
--import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

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
  let connstr = BS.pack $ cfgDbConnStr config
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

    getpost ("login") loginAction

  get root $ maybeUser $ \case
    Just _ ->
      text "Hello User!"
    Nothing ->
      text "Hello World!"

  get ("hello" <//> var) $ \name -> do
    AppState cfg <- getState
    text ("Hello " <> name <> ", welcome to " <> T.pack (show $ cfgTitle cfg))

  prehook authHook $ do
    get ("settings") $ do
      (user :: User) <- fmap findFirst getContext
      text ("Hello " <> T.pack (userName user))

    get ("logout") $ do
      sess <- readSession
      case sess of
        EmptySession ->
          text "Not logged in."
        SessionId _ -> do
          writeSession EmptySession
          text "Logged out."

loginAction :: (ListContains n IsGuest xs, NotInList User xs ~ 'True) => Action (HVect xs) ()
loginAction = do
  form <- runForm "loginForm" FS.signinForm
  let
    formView mErr view = do
      maybe (pure ()) id mErr
      FS.signinFormView view
  case form of
    (view, Nothing) ->
      lucid $ formView Nothing view
    (view, Just FS.Signin{sinLogin, sinPassword}) -> do
      signinRes <-
        runQuery $ Sql.run (getUserWithPass sinLogin sinPassword)
      case signinRes of
        Right Nothing ->
          lucid $ formView (pure $ p_ "Invalid user name/email or password.") view
        Right (Just user) -> do
          writeSession (SessionId (userId user))
          text $ "Logged in as: " <> T.pack (userName user)
        Left err -> do
          text $ T.pack (show err)

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
        text "Unknown user. Login first!"
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
        runQuery $ Sql.run (getUserById uid)
      case emUser of
        Left err ->
          text $ T.pack $ show err
        Right mu ->
          action mu
