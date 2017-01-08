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

registerForm :: Monad m => D.Form [String] m Registration
registerForm = Registration
    <$> "name"      .: D.validateM validateName (D.string Nothing)
    <*> "password1" .: D.validateM validatePass (D.string Nothing)
    <*> "password2" .: D.string Nothing
    <*> "email"     .: D.validateM validateMail (D.string Nothing)


-- @TODO: check against database
validateName :: Monad m => String -> m (D.Result [String] String)
validateName name = validateM name
  [ whenMaybe (length (drop 1 name) == 0) $
      pure $ pure "Name must be at least 2 characters long"
  , whenMaybe (length (drop 50 name) > 0) $
      pure $ pure "Name must not contain more than 50 characters long"
  ]

-- @TODO: check passwords match
validatePass :: Monad m => String -> m (D.Result [String] String)
validatePass pass = validateM pass
  [ whenMaybe (length (drop 4 pass) == 0) $
      pure $ pure "Password must be at least 4 characters long"
  , whenMaybe (length (drop 64 pass) > 0) $
      pure $ pure "Password must not contain more than 64 characters long"
  ]


validateMail :: Monad m => String -> m (D.Result [String] String)
validateMail email = validateM email
  [ whenMaybe (isValidEmail $ T.pack email) $
      pure $ pure "Invalid email address."
  ]


{- | Defining the view for the registeration form
-}

registerFormView :: D.View (H.Html ()) -> H.Html ()
registerFormView view =
  D.form view "register" $ do
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

    D.inputSubmit "Sign-up"


{- | Defining the login form as a digestive functor
-}

data Signin
   = Signin
   { sinLogin :: String
   , sinPassword :: String
   } deriving (Show)

signinForm :: Monad m => D.Form (H.Html ()) m Signin
signinForm = Signin
    <$> "login"    .: D.validateM validateSigninName (D.string Nothing)
    <*> "password" .: D.validateM validateSigninPass (D.string Nothing)

-- @TODO: check against database
validateSigninName :: Monad m => String -> m (D.Result (H.Html ()) String)
validateSigninName name = renderErrs $ validateM name
  [ whenMaybe (length (drop 1 name) == 0) $
      pure $ pure "Name must be at least 2 characters long"
  , whenMaybe (length (drop 50 name) > 0) $
      pure $ pure "Name must not contain more than 50 characters long"
  ]

-- @TODO: check passwords match
validateSigninPass :: Monad m => String -> m (D.Result (H.Html ()) String)
validateSigninPass pass = renderErrs $ validateM pass
  [ whenMaybe (length (drop 4 pass) == 0) $
      pure $ pure "Password must be at least 4 characters long"
  , whenMaybe (length (drop 64 pass) > 0) $
      pure $ pure "Password must not contain more than 64 characters long"
  ]


{- | Defining the view for the login form
-}

signinFormView :: D.View (H.Html ()) -> H.Html ()
signinFormView view =
  D.form view "login" $ do

    H.div_ $ do
      D.errorList "login" view
      D.label     "login" view "Name/Email: "
      D.inputText "login" view

    H.div_ $ do
      D.errorList     "password" view
      D.label         "password" view "Password: "
      D.inputPassword "password" view

    D.inputSubmit "Sign-in"

{- | Utils
-}

validateM :: Monad m => String -> [m (Maybe String)] -> m (D.Result [String] String)
validateM res list = do
  results <- catMaybes <$> sequence list
  if null results
    then pure (D.Success res)
    else pure (D.Error results)

whenMaybe :: Monad m => Bool -> m (Maybe a) -> m (Maybe a)
whenMaybe predicate x
  | predicate = x
  | otherwise = pure Nothing

renderErrs :: Monad m => m (D.Result [String] a) -> m (D.Result (H.Html ()) a)
renderErrs =
  fmap $ D.resultMapError $ H.ul_ . mapM_ (H.li_ . H.toHtml)
