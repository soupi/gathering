{- | Utils for actions

-}

module Web.Gathering.Actions.Utils where

import Data.Text
import Data.Monoid
import Web.Gathering.Html
import Web.Gathering.Types
import Web.Spock.Lucid (lucid)
import Web.Spock

-- | Display the form to the user
formViewer :: Text -> Text -> (t -> Html) -> Maybe Html -> t -> Action v ()
formViewer title actionName form mErr view = do
  lucid $ do
    template
      (title <> " - " <> actionName)
      title
      (pure ())
      $ do
        maybe (pure ()) id mErr
        form view
