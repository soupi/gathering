{- | The entry point to the application

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Gathering.Run where

import Web.Gathering.Types
import Web.Gathering.Config
import Web.Gathering.Database
import Web.Gathering.Auth

import Data.HVect
import Data.Monoid
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
--import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC

import Hasql.Connection (Connection)

import Web.Spock
import Web.Spock.Config

run :: IO ()
run = do
  (config, cmd) <- parseArgs
  print (config, cmd)
  let connstr = BSC.pack $ cfgDbConnStr config
  spockCfg <- defaultSpockCfg EmptySession (PCConn $ hasqlPool connstr) (AppState config)
  case cmd of
    HTTP port ->
      runSpock port (spock spockCfg appRouter)
    HTTPS tls -> do
      runHttps spockCfg tls
    Both port tls -> do
      void $ forkIO $ runSpock port (spock spockCfg appRouter)
      runHttps spockCfg tls

runHttps :: SpockCfg Connection MySession AppState -> TLSConfig -> IO ()
runHttps spockCfg tls = do
  spockApp <- spockAsApp (spock spockCfg appRouter)
  runTLS
    (tlsSettings (tlsKey tls) (tlsCert tls))
    (setPort (tlsPort tls) defaultSettings)
    spockApp


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
