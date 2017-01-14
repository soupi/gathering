{- | Html templates and pages using Lucid

-}


module Web.Gathering.Html where

import qualified Lucid as L
import Lucid.Html5
import Cheapskate.Lucid
import Cheapskate (markdown, Options(..))
import Control.Monad
import Data.Text (Text, pack)
import Data.Monoid

import Web.Gathering.Utils
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

renderEvents :: Text -> Maybe User -> [(Event, [Attendant])] -> Html
renderEvents title mUser eventsAndAtts =
  template
    title
    (L.h1_ "Gathering!")
    (L.nav_ $ navigation mUser)
    (events mUser eventsAndAtts)

navigation :: Maybe User -> Html
navigation mUser = do
  L.ul_ . sequence_ $
    case mUser of
      Just user ->
        [ L.li_ ("Signed-in as " <> L.toHtml (userName user))
        , L.li_ (L.a_ [ L.href_ "/signout" ] "Sign-out")
        ]
        <> [ L.li_ (L.a_ [ L.href_ "/event/new" ] "New Event") | userIsAdmin user ]

      Nothing ->
        [ L.li_ (L.a_ [ L.href_ "/signin" ] "Sign-in")
        , L.li_ (L.a_ [ L.href_ "/signup" ] "Sign-up")
        ]


noEvents :: Text -> Html
noEvents title =
  template
    title
    (L.h1_ "Gathering!")
    (L.ul_ $ pure ())
    (p_ "No available events!")


------------
-- Events --
------------

-- | Render all events + attendants
events :: Maybe User -> [(Event, [Attendant])] -> Html
events mUser =
  mapM_ (\(e,a) -> L.div_ (event mUser e *> attendants (eventId e) a))

-- | Render an event
-- @TODO: render time properly and according to the users' timezone
event :: Maybe User -> Event -> Html
event mUser e = do
  L.h2_ $ do
    L.a_ [href_ ("/event/" <> (pack . show $ eventId e))] $
      L.toHtml $ eventName e
    when (Just True == fmap userIsAdmin mUser) $ do
      " - ("
      L.a_ [href_ ("/event/" <> (pack . show $ eventId e) <> "/edit")] "edit"
      " | "
      L.a_ [href_ ("/event/" <> (pack . show $ eventId e) <> "/delete")] "delete"
      ")"

  L.ul_ $ mapM_ (L.li_ . L.toHtml)
    [ "Location: " <> eventLocation e
    , "Date: "     <> formatDateTime (eventDateTime e)
    , "Duration: " <> formatDiffTime (eventDuration e)
    ]

  L.div_ $ do
    renderDoc $ markdown markdownOptions (eventDesc e)

-- | Render attendant list
attendants :: EventId -> [Attendant] -> Html
attendants eid atts = do
  L.div_ $ do
    L.h2_ "Will you attend?"
    L.ul_ . sequence_ $
      [ L.li_ (L.a_ [ L.href_ $ "/event/" <> pack (show eid) <> "/attending"     ] "Yes")
      , L.li_ (L.a_ [ L.href_ $ "/event/" <> pack (show eid) <> "/not-attending" ] "No")
      ]

  L.div_ $ do
    L.h3_ "Going"
    L.ul_ . mapM_ attendant . filter attendantAttending $ atts

  L.div_ $ do
    L.h3_ "Not Going"
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
