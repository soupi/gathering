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

import Web.Gathering.Utils
import Web.Gathering.Forms.Utils
import Web.Gathering.Html (Html)

import Text.Digestive ((.:))
import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as D
import qualified Lucid as H
import qualified Data.Text as T
import Text.Html.Email.Validate (isValidEmail)

-------------
-- Sign-up --
-------------

{- We want the user to fill their:

- username
- email
- password + password confirmation
- if they want notifications on updates

Will will also include an invisible honey pot to try and detect spam bots.

-}

-- | Definition of a signup data type
data Signup
  = Signup
  { supUsername :: T.Text
  , supEmail :: T.Text
  , supPassword :: T.Text
  , supPasswordConfirm :: T.Text
  , supWantNotifications :: Bool
  , supSpamHoneyPot :: T.Text
  } deriving (Show)

-- | Definition of a form and it's validation
signupForm :: Monad m => T.Text -> D.Form Html m Signup
signupForm csrfToken = const Signup
    <$> "__csrf_token" .: D.text (Just csrfToken)
    <*> "name"      .: D.validateM validateName (fmap (fmap trim) D.text Nothing)
    <*> "email"     .: D.validateM validateMail (fmap (fmap trim) D.text Nothing)
    <*> "password1" .: D.validateM validatePass (fmap (fmap trim) D.text Nothing)
    <*> "password2" .: D.text Nothing
    <*> "get_notifications" .: D.bool Nothing
    <*> "shp" .: D.text Nothing

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

    H.div_ $ do
      D.inputHidden "shp" view

    D.inputHidden "__csrf_token" view

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
signinForm :: Monad m => T.Text -> D.Form Html m Signin
signinForm csrfToken = const Signin
    <$> "__csrf_token" .: D.text (Just csrfToken)
    <*> "login"    .: fmap (fmap trim) D.text Nothing
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

    D.inputHidden "__csrf_token" view

    D.inputSubmit "Sign-in"

