{- | Settings form

The forms will be created using the digestive-functors,
digestive-functors-lucid and the lucid libraries.
To read more about those check the Web.Gathering.Forms.Sign module


-}


module Web.Gathering.Forms.Settings where

import Web.Gathering.Model
import Web.Gathering.Html (Html)

import Text.Digestive ((.:))
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import qualified Lucid as H

--------------
-- Settings --
--------------

-- | Definition of a delete event data type
data Settings
  = Settings
  { wantsUpdates :: Bool
  }
  deriving (Show)

-- | Definition of a form and it's validation
settingsForm :: Monad m => User -> D.Form Html m Settings
settingsForm user = Settings
    <$> "wantsUpdates" .: D.bool (Just $ userWantsUpdates user)


-- | Defining the view for the settings form
settingsFormView :: D.View Html -> Html
settingsFormView view = do
    H.div_ $ do
      D.inputCheckbox "wantsUpdates" view
      D.label         "wantsUpdates" view
        "Send updates to my email account."

    D.inputSubmit "Save"
