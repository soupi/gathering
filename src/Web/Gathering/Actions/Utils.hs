{- | Utils for actions

-}

module Web.Gathering.Actions.Utils where

import Data.Text
import Data.Monoid
import Web.Gathering.Html

-- | Display the form to the user
formViewer :: Text -> Text -> (t -> Html) -> Maybe Html -> t -> Html
formViewer title actionName form mErr view = do
  template
    (title <> " - " <> actionName)
    title
    (pure ())
    $ do
      maybe (pure ()) id mErr
      form view
