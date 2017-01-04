module Web.Gathering where

import Web.Gathering.Types

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T


run :: IO ()
run = do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
    get root $ text "Hello World!"
    get ("hello" <//> var) $ \name -> do
        DummyAppState ref <- getState
        visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
        text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
