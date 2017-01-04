{- | Define Signin, Signup and lost password forms
-}


module Web.Gathering.Forms.Sign where

import Web.Gathering.Types

import Data.Maybe (catMaybes)
import Text.Digestive ((.:))
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import qualified Lucid as H
import qualified Data.Text as T
import Text.Html.Email.Validate (isValidEmail)

{- | Defining the registeration form as a digestive functor
-}

data Registration
   = Registration
   { regUsername :: String
   , regPassword :: String
   , regPasswordConfirm :: String
   , regEmail :: String
   } deriving (Show)

registerForm :: D.Form [String] IO Registration
registerForm = Registration
    <$> "name"      .: D.validateM validateName (D.string Nothing)
    <*> "password1" .: D.validateM validatePass (D.string Nothing)
    <*> "password2" .: D.string Nothing
    <*> "email"     .: D.validateM validateMail (D.string Nothing)


-- @TODO: check against database
validateName :: String -> IO (D.Result [String] String)
validateName name = validateM name
  [ whenMaybe (length (drop 1 name) == 0) $
      pure $ pure "Name must be at least 2 characters long"
  , whenMaybe (length (drop 50 name) > 0) $
      pure $ pure "Name must not contain more than 50 characters long"
  ]

-- @TODO: check passwords match
validatePass :: String -> IO (D.Result [String] String)
validatePass pass = validateM pass
  [ whenMaybe (length (drop 4 pass) == 0) $
      pure $ pure "Password must be at least 4 characters long"
  , whenMaybe (length (drop 64 pass) > 0) $
      pure $ pure "Password must not contain more than 64 characters long"
  ]


validateMail :: String -> IO (D.Result [String] String)
validateMail email = validateM email
  [ whenMaybe (isValidEmail $ T.pack email) $
      pure $ pure "Invalid email address."
  ]


{- | Defining the view for the registeration form
-}

registerFormView :: D.View (H.Html ()) -> H.Html ()
registerFormView view = do
  H.div_ $ do
    D.errorList "name" view
    D.label     "name" view "Name: "
    D.inputText "name" view

  H.div_ $ do
    D.errorList "email" view
    D.label     "email" view "Email: "
    D.inputText "email" view

  H.div_ $ do
    D.errorList     "password1" view
    D.label         "password1" view "Password: "
    D.inputPassword "password1" view

  H.div_ $ do
    D.label         "password2" view "Confirm Password: "
    D.inputPassword "password2" view


{- | Utils
-}

validateM :: String -> [IO (Maybe String)] -> IO (D.Result [String] String)
validateM res list = do
  results <- catMaybes <$> sequence list
  if null results
    then pure (D.Success res)
    else pure (D.Error results)

whenMaybe :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
whenMaybe predicate x
  | predicate = x
  | otherwise = pure Nothing
