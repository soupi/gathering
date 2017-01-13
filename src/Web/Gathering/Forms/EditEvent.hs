{- | Define Event creation

The forms will be created using the digestive-functors,
digestive-functors-lucid and the lucid libraries.
To read more about those check the Web.Gathering.Forms.Sign module


-}


module Web.Gathering.Forms.EditEvent where

import Web.Gathering.Forms.Utils
import Web.Gathering.Html (Html)

import Data.Maybe (isNothing)
import Text.Digestive ((.:))
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import qualified Lucid as H
import qualified Data.Text as T
import Data.Time

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
editEventForm :: Monad m => D.Form Html m EditEvent
editEventForm = EditEvent
    <$> "name"     .: D.check "Cannot be empty" (not . T.null) (fmap (fmap trim) D.text Nothing)
    <*> "desc"     .: D.check "Cannot be empty" (not . T.null) (fmap (fmap trim) D.text Nothing)
    <*> "location" .: D.check "Cannot be empty" (not . T.null) (fmap (fmap trim) D.text Nothing)
    <*> "datetime" .: D.validateM validateDateTime (fmap (fmap trim) D.text Nothing)
    <*> "duration" .: D.validateM validateDuration (fmap (fmap trim) D.text Nothing)

-- | Check we can parse this date time
validateDateTime :: Monad m => T.Text -> m (D.Result Html T.Text)
validateDateTime datetime = validateM datetime
  [ whenMaybe (isNothing $ parseDateTime datetime) $
      pure $ pure "Date time string must be of this format: 'YYYY-MM-DD HH:MM (ZONE)'. for example: '2016-17-04 17:45 (UTC)'."
  ]

-- | Check we can parse this duration
validateDuration :: Monad m => T.Text -> m (D.Result Html T.Text)
validateDuration duration = validateM duration
  [ whenMaybe (isNothing $ toDuration duration) $
      pure $ pure "Duration string must be of this format: 'HH:MM'. for example: '17:45'."
  ]
  where

-- | try to parse the format 'HH:MM' to DiffTime
toDuration :: T.Text -> Maybe DiffTime
toDuration (trim -> T.unpack -> duration) =
  case duration of
    h1:h2:':':m1:m2:[]
      | isDigit h1 && isDigit h2 && read [h1,h2] <= 23
      , isDigit m1 && isDigit m2 && read [m1,m2] <= 59
     -> pure . secondsToDiffTime $ (read [h1,h2] * 60 * 60) + (read [m1,m2] * 60)

    _ -> Nothing
  where
    isDigit = (`elem` ['0'..'9'])




-- | Defining the view for the edit event form
editEventFormView :: T.Text -> D.View Html -> Html
editEventFormView formName view =
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

    D.inputSubmit "Create"


parseDateTime :: T.Text -> Maybe UTCTime
parseDateTime = parseTimeM True defaultTimeLocale "%F %H:%M (%Z)" . T.unpack
