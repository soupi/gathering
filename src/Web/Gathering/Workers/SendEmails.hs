{- | Send emails on

- New user verifications
- New event creations
- Event updates

-}


module Web.Gathering.Workers.SendEmails where

import Prelude hiding (unlines)
import Control.Exception
import Data.Bool (bool)
import Data.Int (Int32)
import Turtle
import Network.Mail.SMTP
import Hasql.Session
import Hasql.Connection
import Control.Monad
import Data.Text (pack)
import Data.Text.Lazy (unlines, fromStrict)
import qualified Data.Set as S
import Lucid (renderText)
import Cheapskate
import Cheapskate.Lucid

import Web.Gathering.Html (markdownOptions)
import Web.Gathering.Model
import Web.Gathering.Utils
import Web.Gathering.Config
import Web.Gathering.Database


newEventsWorker :: AppConfig -> IO ()
newEventsWorker config = forever $ do
  newEventsWorker' config
  sleep (60 * 20) -- sleep for 20 minutes

newEventsWorker' :: AppConfig -> IO ()
newEventsWorker' config = do
  mConn <- acquire (cfgDbConnStr config)
  case mConn of
    Right conn -> do
      sendNewEvents config conn `catch` \ex -> err ("New Events Worker: " <> (pack $ show (ex :: IOException)))
      release conn
    Left ex ->
      err ("New Events Worker: " <> pack (show ex))


sendNewEvents :: AppConfig -> Connection -> IO ()
sendNewEvents config conn = do
  mNewEventsUsers <- flip run conn $
    (,)
      <$> getNewEvents
      <*> fmap (filter $ userWantsUpdates) getUsers

  case mNewEventsUsers of
    Left er -> err (pack $ show er)

    Right (newEvents, users) -> do
      forM_ newEvents $ \case
        ((event, False), _) -> do
          mapM (notifyNewEvent config event False) users

        ((event, True), atts) ->
          let
            getAttsWantsUpdates filt =
              S.map attendantUser $ S.filter (filt . attendantFollowsChanges) (S.fromList atts)

            notifiedUsers =
              S.union (S.fromList users) (getAttsWantsUpdates id) S.\\ (getAttsWantsUpdates not)
          in
            mapM (notifyNewEvent config event True) (S.toList notifiedUsers)

      forM_ newEvents $
        \((ne,_),_) -> run (removeNewEvent ne) conn

notifyNewEvent :: AppConfig -> Event -> Bool -> User -> IO ()
notifyNewEvent config event isEdit user = do
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
          "To read more about the event, [click here](http://"
            <> cfgDomain config
            <> "/event/"
            <> pack (show $ eventId event)
            <> ")."
        , ""
        , renderText . renderDoc . markdown markdownOptions $
          "To find about more events from " <> cfgTitle config <> ", [click here](http://" <> cfgDomain config <> ")!"
        ]
      ]

notifyVerification :: AppConfig -> Int32 -> User -> IO ()
notifyVerification config key user = do
  renderSendMail $
    emailTemplate
      config
      user
      ("Verify your account for " <> cfgTitle config)
      [ htmlPart $ unlines
        [ renderText . renderDoc . markdown markdownOptions $
          "Click the following link to verify your account:\n\n"
        <>"[Verify your account](http://"
            <> cfgDomain config
            <> "/verify-user/"
            <> pack (show key)
            <> "/"
            <> pack (show $ userId user)
            <> ")"
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
