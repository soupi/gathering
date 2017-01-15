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

template :: Text -> Text -> Html -> Html -> Html
template title heading nav body =
  doctypehtml_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]

      title_ (L.toHtml title)

      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]

      link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/normalize.css" ]
      link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/skeleton.css"  ]

    body_ $ do
      div_ [class_ "container"] $ do
        L.div_ [ L.class_ "row" ] $ do
          header_ [ L.class_ "eight columns" ] $
            h1_ (L.a_ [ L.href_ "/" ] $ L.toHtml heading)

          nav_ [class_ "four columns"] nav

        div_ [id_ "main"] body

        footer_ [id_ "footer"] $
          p_ $ do
            "Powered by "
            a_ [href_ "https://github.com/soupi/gathering"] "Gathering"

renderEvents :: Text -> Text -> Maybe User -> [(Event, [Attendant])] -> Html
renderEvents title heading mUser eventsAndAtts =
  template
    title
    heading
    (L.nav_ $ navigation mUser)
    (events mUser eventsAndAtts)

navigation :: Maybe User -> Html
navigation mUser = do
  case mUser of
    Just user -> do
      L.p_ ("Signed-in as " <> L.span_ [ L.class_ "signed-in" ] (L.toHtml (userName user)))
      L.ul_ . sequence_ $
        [ L.li_ (L.a_ [ L.href_ "/event/new" ] "New Event") | userIsAdmin user ]
        <> [ L.li_ (L.a_ [ L.href_ "/signout" ] "Sign-out") ]

    Nothing -> do
      L.p_ (L.toHtmlRaw ("<br>" :: Text))
      L.ul_ . sequence_ $
        [ L.li_ (L.a_ [ L.href_ "/signin" ] "Sign-in")
        , L.li_ (L.a_ [ L.href_ "/signup" ] "Sign-up")
        ]


noEvents :: Text -> Html
noEvents heading =
  template
    (heading <> " - No events")
    heading
    (L.ul_ $ pure ())
    (p_ "No available events!")


------------
-- Events --
------------

-- | Render all events + attendants
events :: Maybe User -> [(Event, [Attendant])] -> Html
events mUser =
  mapM_ (\(e,a) -> L.div_ [ class_ "event-attendants row" ] (event mUser e *> attendants (eventId e) a))

-- | Render an event
-- @TODO: render time properly and according to the users' timezone
event :: Maybe User -> Event -> Html
event mUser e = L.div_ [ class_ "event nine columns" ] $ do
  L.h2_ $ do
    L.a_ [href_ ("/event/" <> (pack . show $ eventId e))] $
      L.toHtml $ eventName e

  when (Just True == fmap userIsAdmin mUser) $ do
    div_ [ class_ "edit-event" ] $ do
      " ("
      L.a_ [href_ ("/event/" <> (pack . show $ eventId e) <> "/edit")] "edit"
      " | "
      L.a_ [href_ ("/event/" <> (pack . show $ eventId e) <> "/delete")] "delete"
      ")"

  L.table_ [ class_ "event-info" ] $ do
    L.tr_ $ do
      L.th_ "Location"
      L.th_ "Date"
      L.th_ "Duration"
    mapM_ (L.td_ . L.toHtml)
      [ eventLocation e
      , formatDateTime (eventDateTime e)
      , formatDiffTime (eventDuration e)
      ]

  L.div_ $ do
    renderDoc $ markdown markdownOptions (eventDesc e)

-- | Render attendant list
attendants :: EventId -> [Attendant] -> Html
attendants eid atts = L.div_ [ class_ "attendants three columns" ] $ do
  L.div_ $ do
    L.h4_ "Are you going?"

    L.ul_ [ class_ "attending" ] . sequence_ $
      [ L.li_ (L.a_ [ L.href_ $ "/event/" <> pack (show eid) <> "/attending"     ] "Yes")
      , "/"
      , L.li_ (L.a_ [ L.href_ $ "/event/" <> pack (show eid) <> "/not-attending" ] "No")
      ]

  L.div_ $ do
    L.h4_ "Going"
    L.ul_ . mapM_ attendant . filter attendantAttending $ atts

  L.div_ $ do
    L.h4_ "Not Going"
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
