{- | Html templates and pages using Lucid

-}


module Web.Gathering.Html where

import qualified Lucid as L
import Lucid.Html5
import Cheapskate.Lucid
import Cheapskate (markdown, Options(..))
--import Data.Time.Format
import Data.Text (Text, pack)
import Data.Monoid
import Data.Map (Map, toList)
import Data.Time

import Web.Gathering.Model



type Html = L.Html ()

template :: Text -> Html -> Html -> Html -> Html
template title header nav body =
  html_ $ do
    head_ $ do
      title_ (L.toHtml title)
      link_ [rel_ "stylesheet", type_ "text/css", href_ "static/style.css"]
    body_ $ do
      div_ [id_ "header"] header
      nav_ [id_ "nav"] nav
      div_ [id_ "main"] body
      footer_ [id_ "footer"] $
        p_ $ do
          "Powered by "
          a_ [href_ "https://github.com/soupi/gathering"] "Gathering"

main :: Text -> [(Event, [Attendant])] -> Html
main title eventsAndAtts =
  template
    title
    (L.h1_ "Gathering!")
    (L.ul_ $ pure ())
    (events eventsAndAtts)

------------
-- Events --
------------

-- | Render all events + attendants
events :: [(Event, [Attendant])] -> Html
events =
  mapM_ (\(e,a) -> L.div_ (event e *> attendants a))

-- | Render an event
-- @TODO: render time properly and according to the users' timezone
event :: Event -> Html
event e = do
  L.h2_ (L.toHtml $ eventName e)
  L.ul_ $ mapM_ (L.li_ . L.toHtml)
    [ "Location: " <> eventLocation e
    , "Date: "     <> pack (formatTime defaultTimeLocale "%T, %F (%Z)" $ eventDateTime e)
    , "Duration: " <> pack (show $ timeToTimeOfDay $ eventDuration e)
    ]
  L.div_ $ do
    renderDoc $ markdown markdownOptions (eventDesc e)

-- | Render attendant list
attendants :: [Attendant] -> Html
attendants atts = do
  L.div_ $ do
    L.h2_ "Going"
    L.ul_ . mapM_ attendant . filter attendantAttending $ atts
  L.div_ $ do
    L.h2_ "Not Going"
    L.ul_ . mapM_ attendant . filter (not . attendantAttending) $ atts

  where
    attendant =
      L.li_ . L.toHtml . userName . attendantUser


-----------
-- Utils --
-----------

markdownOptions :: Options
markdownOptions =
  Options
    { sanitize = True
    , allowRawHtml = True
    , preserveHardBreaks = True
    , debug = False
    }
