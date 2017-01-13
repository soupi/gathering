{- | Various spock actions

-}

module Web.Gathering.Actions where

import Web.Gathering.Types
import Web.Gathering.Database
import qualified Web.Gathering.Html as Html

import qualified Data.Text as T
import qualified Hasql.Session as Sql (run, Session)
import Data.HVect (HVect)

import Web.Spock
import Web.Spock.Lucid

-- | Display the next events. allows the caller to specify which events to get from the database
displayNextEvents :: (Sql.Session [Event]) -> Maybe User -> Action (HVect xs) ()
displayNextEvents getEventsQuery _ = do
  mEventsAndAtts <- runQuery $ Sql.run $ do
    events <- getEventsQuery
    mapM (\e -> (e,) <$> getAttendantsForEvent e) events
  case mEventsAndAtts of
    -- @TODO this is an internal error that we should take care of internally
    Left err -> do
      text $ T.pack (show err)

    Right eventsAA -> do
      lucid $ Html.main "Gathering" eventsAA
