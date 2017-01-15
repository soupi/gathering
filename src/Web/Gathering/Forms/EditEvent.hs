{- | Define Event creation

The forms will be created using the digestive-functors,
digestive-functors-lucid and the lucid libraries.
To read more about those check the Web.Gathering.Forms.Sign module


-}


module Web.Gathering.Forms.EditEvent where

import Web.Gathering.Utils
import Web.Gathering.Model
import Web.Gathering.Forms.Utils
import Web.Gathering.Html (Html)

import Data.Maybe (isNothing)
import Text.Digestive ((.:))
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import qualified Lucid as H
import qualified Data.Text as T

----------------
-- Edit Event --
----------------

-- | Definition of an edit event data type
data EditEvent
  = EditEvent
  { eEventName :: T.Text
  , eEventDesc :: T.Text
  , eEventLocation :: T.Text
  , eEventDateTime :: T.Text
  , eEventDuration :: T.Text
  }
  deriving (Show)

-- | Definition of a form and it's validation
editEventForm :: Monad m => Maybe EditEvent -> D.Form Html m EditEvent
editEventForm mEvent = EditEvent
    <$> "name"     .: D.check "Cannot be empty" (not . T.null) (fmap (fmap trim) D.text (eEventName     <$> mEvent))
    <*> "desc"     .: D.check "Cannot be empty" (not . T.null) (fmap (fmap trim) D.text (eEventDesc     <$> mEvent))
    <*> "location" .: D.check "Cannot be empty" (not . T.null) (fmap (fmap trim) D.text (eEventLocation <$> mEvent))
    <*> "datetime" .: D.validateM validateDateTime (fmap (fmap trim) D.text (eEventDateTime <$> mEvent))
    <*> "duration" .: D.validateM validateDuration (fmap (fmap trim) D.text (eEventDuration <$> mEvent))

-- | Check we can parse this date time
validateDateTime :: Monad m => T.Text -> m (D.Result Html T.Text)
validateDateTime datetime = validateM datetime
  [ whenMaybe (isNothing $ parseDateTime datetime) $
      pure $ pure "Date time string must be of this format: 'YYYY-MM-DD HH:MM (ZONE)'. for example: '2016-17-04 17:45 (UTC)'."
  ]

-- | Check we can parse this duration
validateDuration :: Monad m => T.Text -> m (D.Result Html T.Text)
validateDuration duration = validateM duration
  [ whenMaybe (isNothing $ parseDiffTime duration) $
      pure $ pure "Duration string must be of this format: 'HH:MM'. for example: '17:45'."
  ]
  where


-- | Defining the view for the edit event form
editEventFormView :: T.Text -> T.Text -> D.View Html -> Html
editEventFormView formName submitText view =
  D.form view formName $ do
    H.div_ $ do
      D.errorList "name" view
      D.label     "name" view "Name: "
      D.inputText "name" view

    H.div_ $ do
      D.errorList "location" view
      D.label     "location" view "Location: "
      D.inputText "location" view

    H.div_ $ do
      D.errorList "datetime" view
      D.label     "datetime" view "When: "
      D.inputText "datetime" view

    H.div_ $ do
      D.errorList "duration" view
      D.label     "duration" view "How long: "
      D.inputText "duration" view

    H.div_ $ do
      D.errorList           "desc" view
      D.label               "desc" view "Description: "
      D.inputTextArea
        (Just 60) (Just 80) "desc" view

    D.inputSubmit submitText


eventToEditEvent :: Event -> EditEvent
eventToEditEvent Event { eventName, eventDesc, eventLocation, eventDateTime, eventDuration } =
  EditEvent
    eventName
    eventDesc
    eventLocation
    (formatDateTime eventDateTime)
    (formatDiffTime eventDuration)

