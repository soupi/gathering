{- | Html templates and pages using Lucid

-}


module Web.Gathering.Html where

import qualified Lucid as H
import Lucid.Html5
import Cheapskate.Lucid
import Cheapskate (markdown, Options(..))
import Control.Monad
import Data.Text (Text, pack, unpack)
import Data.Monoid
import Data.List (find)
import Network.URI (parseURI)

import Web.Gathering.Utils
import Web.Gathering.Model
import Web.Gathering.Config


type Html = H.Html ()


-- | A page template
template :: Text -> AppConfig -> Html -> Html -> Html
template title ac nav body =
  doctypehtml_ $ do
    head_ $ do
      meta_ [ charset_ "utf-8" ]

      title_ (H.toHtml $ cfgTitle ac <> " - " <> title)

      meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]

      link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/normalize.css" ]
      link_ [ rel_ "stylesheet", type_ "text/css", href_ "/css/skeleton.css"  ]

    body_ $ do
      div_ [class_ "container"] $ do
        H.div_ [ H.class_ "top row" ] $ do
          header_ [ H.class_ "seven columns" ] $ do
            h1_ (H.a_ [ H.href_ "/" ] $ H.toHtml $ cfgTitle ac)
            h5_ [ H.class_ "top-desc" ] (H.toHtml $ cfgDesc ac)

          nav_ [class_ "five columns"] nav

        div_ [id_ "main"] body

        footer_ [id_ "footer"] $
          p_ $ do
            "Powered by "
            a_ [href_ "https://github.com/soupi/gathering"] "Gathering"

-- | A page with events
renderEvents :: Text -> Text -> AppConfig -> Maybe User -> [(Event, [Attendant])] -> Html
renderEvents csrfToken title ac mUser eventsAndAtts =
  template
    title
    ac
    (H.nav_ $ navigation mUser)
    $ do
      case eventsAndAtts of
        [] -> p_ "No available events!"
        _  -> events csrfToken mUser eventsAndAtts
      H.script_ [ H.src_ "/js/moment.min.js" ] (mempty :: Text)
      H.script_ [ H.src_ "/js/gathering.js" ] (mempty :: Text)


navigation :: Maybe User -> Html
navigation mUser = do
  H.p_ (H.toHtmlRaw ("<br>" :: Text))
  case mUser of
    Just user -> do
      H.p_ ("Signed-in as " <> H.span_ [ H.class_ "signed-in" ] (H.toHtml (userName user)))
      H.ul_ . sequence_ $
        [ H.li_ (H.a_ [ H.href_ "/event/new" ] "New Event") | userIsAdmin user ]
        <> [ H.li_ (H.a_ [ H.href_ "/events/past" ] "Past Events")
           , H.li_ (H.a_ [ H.href_ "/settings" ] "Settings")
           , H.li_ (H.a_ [ H.href_ "/signout"  ] "Sign-out")
           ]

    Nothing -> do
      H.p_ (H.toHtmlRaw ("<br>" :: Text))
      H.ul_ . sequence_ $
        [ H.li_ (H.a_ [ H.href_ "/events/past" ] "Past Events")
        , H.li_ (H.a_ [ H.href_ "/signin" ] "Sign-in")
        , H.li_ (H.a_ [ H.href_ "/signup" ] "Sign-up")
        ]


------------
-- Events --
------------

-- | Render all events + attendants
events :: Text -> Maybe User -> [(Event, [Attendant])] -> Html
events csrfToken mUser =
  mapM_ (\(e,a) -> H.div_ [ class_ "event-attendants row" ] (event mUser e *> attendants csrfToken (eventId e) mUser a))

-- | Render an event
-- @TODO: render time properly and according to the users' timezone
event :: Maybe User -> Event -> Html
event mUser e = H.div_ [ class_ "event nine columns" ] $ do
  H.h2_ $ do
    H.a_ [href_ ("/event/" <> (pack . show $ eventId e))] $
      H.toHtml $ eventName e

  when (Just True == fmap userIsAdmin mUser) $ do
    div_ [ class_ "edit-event" ] $ do
      " ("
      H.a_ [href_ ("/event/" <> (pack . show $ eventId e) <> "/edit")] "edit"
      " | "
      H.a_ [href_ ("/event/" <> (pack . show $ eventId e) <> "/delete")] "delete"
      ")"

  H.table_ [ class_ "event-info" ] $ do
    H.tr_ $ do
      H.th_ "Location"
      H.th_ "Date"
      H.th_ "Duration"
    mapM_ H.td_
      [ renderLocation $ eventLocation e
      , H.span_ [ H.class_ "datetime" ] . H.toHtml $ formatDateTime (eventDateTime e)
      , H.toHtml $ formatDiffTime (eventDuration e)
      ]

  H.div_ $ do
    renderDoc $ markdown markdownOptions (eventDesc e)

-- | Render event Location
renderLocation :: Text -> Html
renderLocation location = 
  case parseURI (unpack location) of
    Just _ -> H.a_ [href_ location] asHtml
    Nothing -> asHtml
  where
    asHtml = H.toHtml location

-- | Render attendant list
attendants :: Text -> EventId -> Maybe User -> [Attendant] -> Html
attendants csrfToken eid mUser atts = H.div_ [ class_ "attendants three columns" ] $ do
  H.div_ $ do
    H.h4_ "Are you going?"

    H.ul_ [ class_ "attending" ] . sequence_ $
      [ securePostLink
        [ class_ "markgreen" | Just True <- [ isAttending ] ]
        csrfToken ("/event/" <> pack (show eid) <> "/attending") "Yes"
      , " "
      , securePostLink
        [ class_ "markred" | Just False <- [ isAttending ] ]
        csrfToken ("/event/" <> pack (show eid) <> "/not-attending") "No"
      ]

  H.div_ $ do
    H.h4_ "Going"
    H.ul_ . mapM_ attendant . filter attendantAttending $ atts

  H.div_ $ do
    H.h4_ "Not Going"
    H.ul_ . mapM_ attendant . filter (not . attendantAttending) $ atts

  where
    attendant :: Attendant -> Html
    attendant (attendantUser -> u) =
      H.li_ [ class_ "markuser" | Just True <- [ (==) (userId u) . userId <$> mUser ] ]
      . H.toHtml
      . userName
      $ u

    isAttending =
      (\uid -> attendantAttending <$> find ((==) uid . (userId . attendantUser)) atts) . userId =<< mUser


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

securePostLink :: [H.Attribute] -> Text -> Text -> Text -> Html
securePostLink attrs csrfToken action btn = do
  H.form_ [ H.class_ "securePostLink", H.enctype_ "application/x-www-form-urlencoded", action_ action, method_ "POST" ] $ do
    H.input_ [ H.type_ "hidden", H.name_ "__csrf_token", H.value_ csrfToken ]
    H.input_ $ [ H.type_ "submit", H.value_ btn ] <> attrs
