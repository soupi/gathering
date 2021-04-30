{- | Send emails to users

Defines a worker that will send emails for:

- New user verifications
- New event creations
- Event updates

and defines functions to send mails for:

- Sign-up verification
- Reset password

-}


module Web.Gathering.Workers.SendEmails where

import Prelude hiding (unlines)
import Control.Exception
import Data.Bool (bool)
import Turtle (sleep)
import Network.Mail.SMTP hiding (htmlPart)
import Hasql.Session
import Hasql.Connection
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Text (pack, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (unlines, fromStrict)
import qualified Data.Text.Lazy as TL
import qualified Data.Set as S
import Lucid (renderText)
import Cheapskate
import Cheapskate.Lucid
import Network.Mail.Mime (Part, Mail, htmlPart)

import Web.Gathering.Html (markdownOptions)
import Web.Gathering.Model
import Web.Gathering.Utils
import Web.Gathering.Config
import Web.Gathering.Types
import Web.Gathering.HashPassword
import Web.Gathering.Database
import Web.Gathering.Workers.Logger


-- | Will run forever and will send emails on new events or event changes every 20 minutes
newEventsWorker :: Bool -> AppState -> IO ()
newEventsWorker sendMailsNow config = do
  unless sendMailsNow $ sleep (60*20)
  forever $ do
    put (appLogger config) "New Events worker starting..."
    eventsWorker' config sendNewEvents
    put (appLogger config) "New Events Worker sleeping..."
    sleep (60 * 20) -- sleep for 20 minutes

-- | Will run forever and will send emails for event reminders every 4 hours
eventRemindersWorker :: Bool -> AppState -> IO ()
eventRemindersWorker sendMailsNow config = do
  unless sendMailsNow $ sleep (60*60)
  forever $ do
    put (appLogger config) "Event Reminders worker starting..."
    eventsWorker' config sendEventReminders
    put (appLogger config) "Event Reminders Worker sleeping..."
    sleep (60 * 60 * 4) -- sleep for 4 hours


-- | Will run action and handle db connection and errors
eventsWorker' :: AppState -> (AppState -> Connection -> IO ()) -> IO ()
eventsWorker' config action = do
  mConn <- acquire (cfgDbConnStr $ appConfig config)
  case mConn of
    Right conn -> do
      action config conn `catch` \ex -> err (appLogger config) ("Events Worker: " <> (pack $ show (ex :: SomeException)))
      release conn
    Left (fromMaybe "Unknown error" -> ex) ->
      err (appLogger config) ("Events Worker: " <> decodeUtf8 ex)

-- | Search the events that messages should be sent for them and send the messages
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


sendEventReminders :: AppState -> Connection -> IO ()
sendEventReminders config conn = do
  mNearEvents <- flip run conn $
      runReadTransaction getNearEvents

  case mNearEvents of
    Left er -> err (appLogger config) ("Near Events Worker: " <> pack (show er))

    Right events ->
      forM_ events $ \case
        (event, atts) ->
          let
            attsWantsUpdates =
              map attendantUser $ filter attendantFollowsChanges atts
          in
            mapM (notifyEventReminder config event) attsWantsUpdates


-- | Send new event or event edited message
notifyNewEvent :: AppState -> Connection -> Event -> Bool -> User -> IO (Either Error ())
notifyNewEvent state@(AppState config _ _) conn event isEdit user = do
  renderSendMail $
    emailTemplate
      config
      user
      (bool "New event @ " "Event updated @ " isEdit <> cfgTitle config <> ": '" <> eventName event <> "'")
      [ htmlPart . unlines $
        [ bool "A new event has been announced:" "This event has been updated:" isEdit
        , "\n"
        , fromStrict $ "<h2>" <> eventName event <> "</h2>"
        , "\n"
        , fromStrict $ "<p><b>Location</b>: " <> eventLocation event
        , "</p>\n"
        , fromStrict $ "<p><b>Datetime</b>: " <> formatDateTime (eventDateTime event)
        , "</p>\n"
        , fromStrict $ "<p><b>Duration</b>: " <> formatDiffTime (eventDuration event)
        , "</p>\n"
        , renderText . renderDoc . markdown markdownOptions $
          "To read more about the event, [click here](" <> getDomain state
            <> "/event/"
            <> pack (show $ eventId event)
            <> ")."
        , ""
        , "---"
        , "\n"
        , renderText . renderDoc . markdown markdownOptions $ eventDesc event
        , "\n"
        , "---"
        , "\n"
        , renderText . renderDoc . markdown markdownOptions $
          "To find about more events from " <> cfgTitle config <> ", [click here](" <> getDomain state <> ")!"
        ]
        <> unsubscribe state user
      ]
  run (runWriteTransaction $ removeNewEvent event) conn

-- | Send an event reminder message
notifyEventReminder :: AppState -> Event -> User -> IO ()
notifyEventReminder state@(AppState config _ _) event user = do
  diffTime <- fmap ((`div` 60) . floor . (eventDateTime event `diffUTCTime`)) getCurrentTime
  let
    diffHours = diffTime `div` 60
    diffMins  = diffTime `mod` 60
    diffTimeStr
      | diffHours == 1 = "1 hour and " <> show diffMins <> " minutes"
      | otherwise  = show diffHours <> " hours and " <> show diffMins <> " minutes"

  renderSendMail $
    emailTemplate
      config
      user
      ("Reminder: '" <> eventName event
       <> "' @ " <> cfgTitle config
       <> " is taking place in " <> pack diffTimeStr
      )
      [ htmlPart . unlines $
        [ "<p>"
        , "This is a reminder that the following event is taking place in " <> TL.pack diffTimeStr
        , "</p>\n"
        , fromStrict $ "<h2>" <> eventName event <> "</h2>"
        , "\n"
        , fromStrict $ "<p><b>Location</b>: " <> eventLocation event
        , "</p>\n"
        , fromStrict $ "<p><b>Datetime</b>: " <> formatDateTime (eventDateTime event)
        , "</p>\n"
        , fromStrict $ "<p><b>Duration</b>: " <> formatDiffTime (eventDuration event)
        , "</p>\n"
        , renderText . renderDoc . markdown markdownOptions $
          "To read more about the event, [click here](" <> getDomain state
            <> "/event/"
            <> pack (show $ eventId event)
            <> ")."
        , "\n"
        , "---"
        , "\n"
        , renderText . renderDoc . markdown markdownOptions $ eventDesc event
        , "\n"
        , "---"
        , "\n"
        , renderText . renderDoc . markdown markdownOptions $
          "To find about more events from " <> cfgTitle config <> ", [click here](" <> getDomain state <> ")!"
        ]
        <> unsubscribe state user
      ]



-- | Send a verification message to a newly registered user
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
        , "Note that this link will expire in two days."
        , ""
        , "If this wasn't you, feel free to ignore it!"
        ]
      ]

-- | Send a link to a reset password form
notifyResetPassword :: AppState -> User -> Text -> IO ()
notifyResetPassword state@(AppState config _ _) user hash = do
  renderSendMail $
    emailTemplate
      config
      user
      ("Reset your password for " <> cfgTitle config)
      [ htmlPart . unlines $
        [ renderText . renderDoc . markdown markdownOptions $
          "You have requested a password reset link:\n\n"
        <>"[Click here to reset your password]("
            <> getDomain state
            <> "/reset-password/"
            <> hash
            <> "/"
            <> userEmail user
            <> ")"
        , ""
        , "Note that this link will expire in one day."
        , "\n"
        ]
      ]

unsubscribe :: AppState -> User -> [TL.Text]
unsubscribe state@(AppState config _ _) user =
  [ "\n"
  , "---"
  , "\n"
  , renderText . renderDoc $ markdown markdownOptions
      ("If you do not want to receive emails from "
       <> cfgTitle config
       <> ", [Unsubscribe here]("
       <> getDomain state <> "/unsubscribe/" <> userEmail user <> "/" <> hashMD5 (userHash user) (userEmail user)
       <> ")."
      )
  ]

-- | A template for emails
emailTemplate :: AppConfig -> User -> Text -> [Part] -> Mail
emailTemplate config user =
  simpleMail
    (Address (Just $ cfgTitle config) "noreply")
    [Address (Just $ userName user) (userEmail user)]
    []
    []

-- | Render the protocol, domain and port of the website
getDomain :: AppState -> Text
getDomain (AppState config cmd _) =
  getProtocol cmd
   <> "://"
   <> cfgDomain config
   <> ":" <> (pack . show $ getPort cmd)
