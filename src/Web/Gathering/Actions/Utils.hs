{- | Utils for actions

-}

module Web.Gathering.Actions.Utils where

import Web.Gathering.Html
import Web.Gathering.Types
import Web.Gathering.Config
import qualified Web.Gathering.Workers.Logger as L

import Data.Text (Text)
import Web.Spock
import Web.Spock.Lucid (lucid)
import Control.Monad.IO.Class (liftIO)

-- | Display the form to the user
formViewer :: AppConfig -> Text -> Html -> Maybe Html -> Action v ()
formViewer ac actionName form mErr = do
  lucid $ do
    template
      actionName
      ac
      (pure ())
      $ do
        maybe (pure ()) id mErr
        form

-- | Log a message
put :: Text -> Action v ()
put msg = do
  q <- appLogger <$> getState
  liftIO $ L.act L.Put q msg

-- | Log a warning
warn :: Text -> Action v ()
warn msg = do
  q <- appLogger <$> getState
  liftIO $ L.act L.Warn q msg

-- | Log an error
err :: Text -> Action v ()
err msg = do
  q <- appLogger <$> getState
  liftIO $ L.act L.Error q msg
