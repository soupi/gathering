{- | Send emails on

- New user verifications
- New event creations
- Event updates

-}


module Web.Gathering.Workers.SendEmails where

import Prelude hiding (unlines)
import Control.Exception
import Data.Bool (bool)
import Turtle (sleep)
import Network.Mail.SMTP
import Hasql.Session
import Hasql.Connection
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (unlines, fromStrict)
import qualified Data.Set as S
import Lucid (renderText)
import Cheapskate
import Cheapskate.Lucid

import Web.Gathering.Html (markdownOptions)
import Web.Gathering.Model
import Web.Gathering.Utils
import Web.Gathering.Config
import Web.Gathering.Types
import Web.Gathering.Database
import Web.Gathering.Workers.Logger


newEventsWorker :: AppState -> IO ()
newEventsWorker config = forever $ do
  put (appLogger config) "Events worker starting..."
  newEventsWorker' config
  put (appLogger config) "Events Worker sleeping..."
  sleep (60 * 20) -- sleep for 20 minutes

newEventsWorker' :: AppState -> IO ()
newEventsWorker' config = do
  mConn <- acquire (cfgDbConnStr $ appConfig config)
  case mConn of
    Right conn -> do
      sendNewEvents config conn `catch` \ex -> err (appLogger config) ("New Events Worker: " <> (pack $ show (ex :: SomeException)))
      release conn
    Left (fromMaybe "Unknown error" -> ex) ->
      err (appLogger config) ("New Events Worker: " <> decodeUtf8 ex)


sendNewEvents :: AppState -> Connection -> IO ()
sendNewEvents config conn = do
  mNewEventsUsers <- flip run conn $
    (,)
      <$> runReadTransaction getNewEvents
      <*> (filter userWantsUpdates <$> runReadTransaction getUsers)

  case mNewEventsUsers of
    Left er -> err (appLogger config) ("New Events Worker: " <> pack (show er))

    Right (newEvents, users) ->
      forM_ newEvents $ \case
        ((event, False), _) -> do
          mapM (notifyNewEvent config conn event False) users

        ((event, True), atts) ->
          let
            getAttsWantsUpdates filt =
              S.map attendantUser $ S.filter (filt . attendantFollowsChanges) (S.fromList atts)

            notifiedUsers =
              S.union (S.fromList users) (getAttsWantsUpdates id) S.\\ (getAttsWantsUpdates not)
          in
            mapM (notifyNewEvent config conn event True) (S.toList notifiedUsers)

notifyNewEvent :: AppState -> Connection -> Event -> Bool -> User -> IO (Either Error ())
notifyNewEvent state@(AppState config _ _) conn event isEdit user = do
  renderSendMail $
    emailTemplate
      config
      user
      (bool "New event @" "Event updated @" isEdit <> cfgTitle config <> ": '" <> eventName event <> "'")
      [ htmlPart $ unlines
        [ bool "A new event has been announced:" "This event has been updated:" isEdit
        , ""
        , fromStrict $ eventName event
        , ""
        , renderText . renderDoc . markdown markdownOptions $ eventDesc event
        , ""
        , fromStrict $ "Location: " <> eventLocation event
        , ""
        , fromStrict $ "Date/Time: " <> formatDateTime (eventDateTime event)
        , ""
        , fromStrict $ "Duration: " <> formatDiffTime (eventDuration event)
        , ""
        , renderText . renderDoc . markdown markdownOptions $
          "To read more about the event, [click here](" <> getDomain state
            <> "/event/"
            <> pack (show $ eventId event)
            <> ")."
        , ""
        , renderText . renderDoc . markdown markdownOptions $
          "To find about more events from " <> cfgTitle config <> ", [click here](" <> getDomain state <> ")!"
        ]
      ]
  run (runWriteTransaction $ removeNewEvent event) conn

getDomain :: AppState -> Text
getDomain (AppState config cmd _) =
  getProtocol cmd
   <> "://"
   <> cfgDomain config
   <> ":" <> (pack . show $ getPort cmd)

notifyVerification :: AppState -> User -> IO ()
notifyVerification state@(AppState config _ _) user = do
  renderSendMail $
    emailTemplate
      config
      user
      ("Verify your account for " <> cfgTitle config)
      [ htmlPart $ unlines
        [ renderText . renderDoc . markdown markdownOptions $
          "Click the following link to verify your account:\n\n"
        <>"[Verify your account]("
            <> getDomain state
            <> "/verify-user/"
            <> pack (show $ userId user)
            <> "/"
            <> userEmail user
            <> ")"
        , ""
        , "Note that this verification link will expire in two days."
        , ""
        , "If this wasn't you, feel free to ignore it!"
        ]
      ]


emailTemplate config user subject content =
  simpleMail
    (Address (Just $ cfgTitle config) "noreply")
    [Address (Just $ userName user) (userEmail user)]
    []
    []
    subject
    $ content
