{- | Utils for digestive functors

-}


module Web.Gathering.Forms.Utils where

import Web.Spock

import Web.Gathering.Types
import Web.Gathering.Html (Html)

import Data.Maybe (catMaybes)
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import qualified Lucid as H
import qualified Data.Text as T

-----------
-- Utils --
-----------

-- | Gets the user input and a list of validation actions to check and it
--   will return a the user input or an Html with all the errors
validateM :: Monad m => T.Text -> [m (Maybe T.Text)] -> m (D.Result Html T.Text)
validateM res list = renderErrs $ do
  results <- catMaybes <$> sequence list
  if null results
    then pure (D.Success res)
    else pure (D.Error results)

-- | map over the [Text] part to convert to an Html view of all errors
renderErrs :: Monad m => m (D.Result [T.Text] a) -> m (D.Result Html a)
renderErrs =
  fmap $ D.resultMapError $ H.ul_ . mapM_ (H.li_ . H.toHtml)

-- | a when for Maybe
whenMaybe :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
whenMaybe predicate x
  | predicate = x
  | otherwise = pure Nothing


-- | A form with hidden csrf token
secureForm :: T.Text -> (D.View Html -> Html) -> D.View Html -> Action v Html
secureForm route formHtml view = do
  csrfToken <- getCsrfToken
  pure $ D.form view route $ do
    H.input_ [ H.type_ "hidden", H.name_ "__csrf_token", H.value_ csrfToken ]
    formHtml view
