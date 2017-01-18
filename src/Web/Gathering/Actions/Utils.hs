{- | Utils for actions

-}

module Web.Gathering.Actions.Utils where

import Data.Text
import Data.Monoid
import Web.Gathering.Html
import Web.Gathering.Types
import Web.Spock.Lucid (lucid)

-- | Display the form to the user
formViewer :: Text -> Text -> Html -> Maybe Html -> Action v ()
formViewer title actionName form mErr = do
  lucid $ do
    template
      (title <> " - " <> actionName)
      title
      (pure ())
      $ do
        maybe (pure ()) id mErr
        form
