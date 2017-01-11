{- | Define Signin, Signup and lost password forms

The forms will be created using the digestive-functors,
digestive-functors-lucid and the lucid libraries.

digestive-functors let's us define a high level description
of our forms including the needed validation needed for each field.
We also define how we'd like the form to look using digestive-functors-lucid library
which maps the corresponding fields we defined in the form to a view, and the errors
for each field as well.

These two + spock-digestive will basically do all the work for us,
it'll present the page and will give us a Maybe <Form> result which we can
either display back to the user with the relevant errors or use it the way we need

-}


module Web.Gathering.Forms.Sign where

import Web.Gathering.Html (Html)

import Data.Maybe (catMaybes)
import Text.Digestive ((.:))
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import qualified Lucid as H
import qualified Data.Text as T
import Text.Html.Email.Validate (isValidEmail)

-------------
-- Sign-up --
-------------

-- | Definition of a signup data type
data Signup
   = Signup
   { supUsername :: T.Text
   , supEmail :: T.Text
   , supPassword :: T.Text
   , supPasswordConfirm :: T.Text
   , supWantNotifications :: Bool
   } deriving (Show)

-- | Definition of a form and it's validation
signupForm :: Monad m => D.Form Html m Signup
signupForm = Signup
    <$> "name"      .: D.validateM validateName (fmap (fmap trim) D.text Nothing)
    <*> "email"     .: D.validateM validateMail (fmap (fmap trim) D.text Nothing)
    <*> "password1" .: D.validateM validatePass (fmap (fmap trim) D.text Nothing)
    <*> "password2" .: D.text Nothing
    <*> "get_notifications" .: D.bool Nothing

-- | trim and check name length for now
validateName :: Monad m => T.Text -> m (D.Result Html T.Text)
validateName name = validateM name
  [ whenMaybe (T.length (T.drop 1 name) == 0) $
      pure $ pure "Name must be at least 2 characters long"
  , whenMaybe (T.length (T.drop 50 name) > 0) $
      pure $ pure "Name must be at least 50 characters long"
  ]

-- | trim and check password length for now
validatePass :: Monad m => T.Text -> m (D.Result Html T.Text)
validatePass pass = validateM pass
  [ whenMaybe (T.length (T.drop 3 pass) == 0) $
      pure $ pure "Password must be at least 4 characters long"
  , whenMaybe (T.length (T.drop 64 pass) > 0) $
      pure $ pure "Password must be at most 64 characters long"
  ]

-- | trim and Validate an email address
validateMail :: Monad m => T.Text -> m (D.Result Html T.Text)
validateMail email = validateM email
  [ whenMaybe (not $ isValidEmail email) $
      pure $ pure "Invalid email address."
  ]


-- | Defining the view for the signup form
signupFormView :: D.View Html -> Html
signupFormView view =
  D.form view "signup" $ do
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

    H.div_ $ do
      D.inputCheckbox "get_notifications" view
      D.label         "get_notifications" view "Get notifications by mail"


    D.inputSubmit "Sign-up"

-------------
-- Sign-in --
-------------

-- | Defining the signin form as a digestive functor
data Signin
   = Signin
   { sinLogin :: T.Text
   , sinPassword :: T.Text
   } deriving (Show)

-- | Defining the form
signinForm :: Monad m => D.Form Html m Signin
signinForm = Signin
    <$> "login"    .: fmap (fmap trim) D.text Nothing
    <*> "password" .: fmap (fmap trim) D.text Nothing

-- | Defining the view for the signin form
signinFormView :: D.View Html -> Html
signinFormView view =
  D.form view "signin" $ do

    H.div_ $ do
      D.errorList "login" view
      D.label     "login" view "Name/Email: "
      D.inputText "login" view

    H.div_ $ do
      D.errorList     "password" view
      D.label         "password" view "Password: "
      D.inputPassword "password" view

    D.inputSubmit "Sign-in"

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

-- | trim spaces from both sides
trim :: T.Text -> T.Text
trim = T.reverse . T.dropWhile (==' ') . T.reverse . T.dropWhile (==' ')
