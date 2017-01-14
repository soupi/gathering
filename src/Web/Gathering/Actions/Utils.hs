{- | Utils for actions

-}

module Web.Gathering.Actions.Utils where

-- | Display the form to the user
formViewer :: Monad m => (t -> m b) -> Maybe (m ()) -> t -> m b
formViewer form mErr view = do
  maybe (pure ()) id mErr
  form view

