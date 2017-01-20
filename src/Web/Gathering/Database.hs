{- | Module describing the database interaction

functions that return `Session <result>` can be run with
`Web.Spock.runQuery . Hasql.Session.run`

Queries and Insert/Upsert queries are the interesting ones.

-}

module Web.Gathering.Database where

import Web.Gathering.Model
import Web.Gathering.Utils

import Web.Spock.Config
import qualified Web.Spock.Config as SC
import qualified Hasql.Connection as Sql
import qualified Hasql.Query as Sql
import qualified Hasql.Session as Sql hiding (query)
import qualified Hasql.Decoders as SqlD
import qualified Hasql.Encoders as SqlE
import qualified Hasql.Transaction as Sql
import qualified Hasql.Transaction.Sessions as Sql

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Functor.Contravariant (contramap)
import Data.Monoid
import Data.Int (Int32)
import Data.List (find)
import Data.Maybe (mapMaybe)


runReadTransaction :: Sql.Transaction a -> Sql.Session a
runReadTransaction = Sql.transaction Sql.Serializable Sql.Read

runWriteTransaction :: Sql.Transaction a -> Sql.Session a
runWriteTransaction = Sql.transaction Sql.Serializable Sql.Write

-----------
-- Query --
-----------

-- | Retrieve a user from the new_users table. Will have the verification random in the userId field.
--   Will return Nothing if the user does not exist
getNewUser :: T.Text -> T.Text -> Sql.Transaction (Maybe User)
getNewUser name email =
  Sql.query (name, email) $
    Sql.statement
      "select verification_rand, user_name, user_email, user_isadmin, user_wants_updates from new_users where (user_name = $1 or user_email = $2) and valid_until > now()"
      (contramap fst (SqlE.value SqlE.text) <> contramap snd (SqlE.value SqlE.text))
      (SqlD.maybeRow decodeUser)
      True

-- | Get a user from the users table with a specific id
getUserById :: UserId -> Sql.Transaction (Maybe User)
getUserById uid = Sql.query uid $
  Sql.statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates from users where user_id = $1"
    (SqlE.value SqlE.int4)
    (SqlD.maybeRow decodeUser)
    True

-- | Find a user by matching either their username or email
getUserLogin :: T.Text -> T.Text -> Sql.Transaction (Maybe (User, BS.ByteString))
getUserLogin name email = Sql.query (name, email) $
  Sql.statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates, user_password_hash from users where user_name = $1 OR user_email = $2"
    (  fst `contramap` SqlE.value SqlE.text
    <> snd `contramap` SqlE.value SqlE.text
    )
    (SqlD.maybeRow ((,) <$> decodeUser <*> SqlD.value SqlD.bytea))
    True

-- | Get a user from the users table from a session
getUserBySession :: UserId -> Sql.Transaction (Maybe User)
getUserBySession uid = Sql.query uid $
  Sql.statement
    "select users.user_id, users.user_name, users.user_email, users.user_isadmin, users.user_wants_updates from users join sessions on users.user_id = sessions.user_id and sessions.valid_until > now() and users.user_id = $1"
    (SqlE.value SqlE.int4)
    (SqlD.maybeRow decodeUser)
    True


-- | Get users from the users table
getUsers :: Sql.Transaction [User]
getUsers = Sql.query () $
  Sql.statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates from users"
    mempty
    (SqlD.rowsList decodeUser)
    False

-- | Get events from the events table
getEvents :: Sql.Transaction [Event]
getEvents = Sql.query () $
  Sql.statement
    "select * from events"
    mempty
    (SqlD.rowsList decodeEvent)
    False

-- | Get future events from the events table
getFutureEvents :: Sql.Transaction [Event]
getFutureEvents = Sql.query () $
  Sql.statement
    "select * from events where event_datetime >= now() order by event_datetime asc"
    mempty
    (SqlD.rowsList decodeEvent)
    False

-- | Get future events from the events table
getPastEvents :: Sql.Transaction [Event]
getPastEvents = Sql.query () $
  Sql.statement
    "select * from events where event_datetime < now() order by event_datetime desc"
    mempty
    (SqlD.rowsList decodeEvent)
    False

-- | Get an event by id from the events table
getEventById :: Int32 -> Sql.Transaction (Maybe Event)
getEventById eid = Sql.query eid $
  Sql.statement
    "select * from events where event_id = $1"
    (SqlE.value SqlE.int4)
    (SqlD.maybeRow decodeEvent)
    False

getNewEvents :: Sql.Transaction [((Event, Bool), [Attendant])]
getNewEvents = do
  events <- Sql.query () $
    Sql.statement
      "select e.*, en.is_edit from events e inner join new_events en on e.event_id = en.event_id where send_after < now()"
      mempty
      (SqlD.rowsList $ (,) <$> decodeEvent <*> SqlD.value SqlD.bool)
      False

  results <- mapM (\e -> (e,) <$> getAttendantsForEvent (fst e)) events

  pure results


-- | Get all attendants
getAttendants :: Sql.Transaction Attendants
getAttendants = do
  attendants <- Sql.query () $
    Sql.statement
      "select * from attendants"
      mempty
      (SqlD.rowsList decodeAttendant)
      False
  events <- filter ((`elem` map fst4 attendants) . eventId) <$> getEvents
  users <- filter ((`elem` map snd4 attendants) . userId) <$> getUsers
  let atts' = mapMaybe (fmap (fmap (:[])) . toAttendant events users) attendants -- @TODO: Danger! ignoring bad values
  pure $
    M.unionsWith (<>) $ map (M.fromList . (:[])) atts'


-- | Get attendants for an event
getAttendantsForEvent :: Event -> Sql.Transaction [Attendant]
getAttendantsForEvent event = do
  Sql.query event $
    Sql.statement
      "select u.user_id, u.user_name, u.user_email, u.user_isadmin, u.user_wants_updates, a.attending, a.follow_changes from attendants a inner join users u on a.user_id = u.user_id where a.event_id = $1"
      (eventId `contramap` SqlE.value SqlE.int4)
      (SqlD.rowsList decodeAttendantUser)
      True


-------------------
-- Insert/Update --
-------------------

-- User --

-- | Add a new user to the new_users table. this will be marked valid for two days
newUser :: User -> BS.ByteString -> Sql.Transaction (Either T.Text User)
newUser user pass = do
  userExists <- getUserLogin (userName user) (userEmail user)
  case userExists of
    Just _ ->
      pure $ Left "This user has already been verified"

    Nothing -> do
      newUserExists <- getNewUser (userName user) (userEmail user)
      case newUserExists of
        Just _ ->
          pure $ Left "User is still pending verification."

        Nothing -> do
          Sql.query (user, pass) $
            Sql.statement
              "insert into new_users (verification_rand, user_name, user_email, user_isadmin, user_wants_updates, user_password_hash, valid_until) values (round(random() * 2000000000), $1, $2, $3, $4, $5, now() + '2 days')"
              encodeNewUser
              SqlD.unit
              True
          r <- getNewUser (userName user) (userEmail user)
          maybe
            (pure $ Left "Internal error: Could not find user after insert.")
            (pure . Right)
            r


verifyNewUser :: Int32 -> T.Text -> Sql.Transaction (Either T.Text User)
verifyNewUser key email = do
  result <- getUserLogin " " email
  case result of
    Just _ ->
      pure (Left "This user has already been verified")
    Nothing -> do
      insertResult <- Sql.query (key, email) $
        Sql.statement
          "insert into users (user_name, user_email, user_isadmin, user_wants_updates, user_password_hash) select user_name, user_email, user_isadmin, user_wants_updates, user_password_hash from new_users where verification_rand = $1 and user_email = $2 and valid_until > now() returning user_id, user_name, user_email, user_isadmin, user_wants_updates"
          (contramap fst (SqlE.value SqlE.int4) <> contramap snd (SqlE.value SqlE.text))
          (SqlD.maybeRow decodeUser)
          True

      case insertResult of
        Just r ->
          pure (pure r)
        Nothing ->
          pure (Left "Could not find associated email. Maybe verification expired?")


-- | Update an existing user in the users table
updateUser :: User -> Sql.Transaction User
updateUser user = do
  Sql.query user $
    Sql.statement
      "update users set user_name = $2, user_email = $3, user_isadmin = $4, user_wants_updates = $5 where user_id = $1"
      encodeExistingUser
      SqlD.unit
      True
  result <- getUserLogin (userName user) (userEmail user)
  maybe
    (fail "Internal error: Could not find user after update.")
    (pure . fst)
    result

-- | Give admin to user by name or email
changeAdminForUser :: Bool -> T.Text -> Sql.Transaction (Either T.Text ())
changeAdminForUser isAdmin login = do
  mUser <- getUserLogin login login
  case mUser of
    Nothing ->
      pure $ Left "User not found."
    Just (user,_) ->
      const (pure ()) <$> updateUser (user { userIsAdmin = isAdmin })

-- | Delete user by name or email from system
deleteUser :: T.Text -> Sql.Transaction (Either T.Text ())
deleteUser login = do
  mUser <- getUserLogin login login
  case mUser of
    Nothing ->
      pure $ Left "User not found."
    Just (user,_) -> do
      Sql.query (userId user) $
        Sql.statement
          "delete from attendants where user_id = $1"
          (SqlE.value SqlE.int4)
          SqlD.unit
          True

      Sql.query (userId user) $
        Sql.statement
          "delete from users where user_id = $1"
          (SqlE.value SqlE.int4)
          SqlD.unit
          True

      pure $ pure ()

-- | Update an existing user in the users table including the password
updateUserWithPassword :: User -> BS.ByteString -> Sql.Transaction User
updateUserWithPassword user pass = do
  Sql.query (user, pass) $
    Sql.statement
      "update users set user_name = $2, user_email = $3, user_isadmin = $4, user_wants_updates = $5, user_password_hash = $6 where user_id = $1"
      encodeExistingUserWithPassword
      SqlD.unit
      True
  result <- getUserLogin (userName user) (userEmail user)
  maybe
    (fail "Internal error: Could not find user after update.")
    (pure . fst)
    result

-- | Delete a user from the new_users table based on their email and verification_rand
removeNewUser :: User -> Sql.Transaction ()
removeNewUser user = do
  Sql.query (userId user, userEmail user) $
    Sql.statement
      "delete from new_users where verification_rand = $1 and user_email = $2"
      (contramap fst (SqlE.value SqlE.int4) <> contramap snd (SqlE.value SqlE.text))
      SqlD.unit
      True

-- Event --

-- | Add a new event to events table
newEvent :: Event -> Sql.Transaction EventId
newEvent event = do
  eid <- Sql.query event $
    Sql.statement
      "insert into events (event_name, event_description, event_location, event_datetime, event_duration) values ($1, $2, $3, $4, $5) returning event_id"
      encodeNewEvent
      (SqlD.singleRow $ SqlD.value SqlD.int4)
      True

  Sql.query eid $
    Sql.statement
      "insert into new_events values ($1, false, now() + '15 minutes')"
      (SqlE.value SqlE.int4)
      SqlD.unit
      True

  pure eid


-- | Update an existing event in the events table
updateEvent :: Event -> Sql.Transaction ()
updateEvent event = do
  Sql.query event $
    Sql.statement
      "update events set event_name = $2, event_description = $3, event_location = $4, event_datetime = $5, event_duration = $6 where event_id = $1"
      encodeExistingEvent
      SqlD.unit
      True

  Sql.query (eventId event) $
    Sql.statement
      "insert into new_events values ($1, true, now() + '15 minutes') on conflict (event_id) do update set is_edit = EXCLUDED.is_edit, send_after = EXCLUDED.send_after"
      (SqlE.value SqlE.int4)
      SqlD.unit
      True

-- | Remove an existing event from the events table
removeEvent :: Event -> Sql.Transaction ()
removeEvent event = do
  removeAttendants event
  removeNewEvent event
  Sql.query (eventId event) $
    Sql.statement
      "delete from events where event_id = $1"
      (SqlE.value SqlE.int4)
      SqlD.unit
      True

removeNewEvent :: Event -> Sql.Transaction ()
removeNewEvent event =
  Sql.query (eventId event) $
    Sql.statement
      "delete from new_events where event_id = $1"
      (SqlE.value SqlE.int4)
      SqlD.unit
      True


-- Attendant --

-- | Add or update an attendant for an event in the attendants table
upsertAttendant :: Event -> Attendant -> Sql.Transaction ()
upsertAttendant event att = Sql.query (event, att) $
  Sql.statement
    "insert into attendants values ($1, $2, $3, $4) on conflict (event_id, user_id) do update set attending = EXCLUDED.attending, follow_changes = EXCLUDED.follow_changes"
    encodeAttendant
    SqlD.unit
    True

-- | Remove an attendant for an event from the attendants table
removeAttendant :: Event -> User -> Sql.Transaction ()
removeAttendant event user = Sql.query (event, user) $
  Sql.statement
    "delete from attendants where event_id = $1 and user_id = $2"
    encodeEventUserIds
    SqlD.unit
    True

-- | Remove all attendants for an event from the attendants table
removeAttendants :: Event -> Sql.Transaction ()
removeAttendants event = Sql.query (eventId event) $
  Sql.statement
    "delete from attendants where event_id = $1"
    (SqlE.value SqlE.int4)
    SqlD.unit
    True

-- UserSession --

-- | Add or update a user session in the sessions table. Set to expire after 1 month
upsertUserSession :: UserId -> Sql.Transaction ()
upsertUserSession uid = Sql.query uid $
  Sql.statement
    "insert into sessions values ($1, now() + '1 month') on conflict (user_id) do update set valid_until = EXCLUDED.valid_until"
    (SqlE.value SqlE.int4)
    SqlD.unit
    True


-- | Kill a session for a user
killSession :: UserId -> Sql.Transaction ()
killSession uid = Sql.query uid $
  Sql.statement
    "delete from sessions where user_id = $1"
    (SqlE.value SqlE.int4)
    SqlD.unit
    True

-- | Delete sessions which have expired
cleanOldSessions :: Sql.Transaction ()
cleanOldSessions = Sql.query () $
  Sql.statement
    "delete from sessions where valid_until < now()"
    mempty
    SqlD.unit
    True

cleanOldNewUsers :: Sql.Transaction ()
cleanOldNewUsers = Sql.query () $
  Sql.statement
    "delete from new_users where valid_until < now()"
    mempty
    SqlD.unit
    True

-- Lost passwords --

requestResetPassword :: T.Text -> Sql.Transaction (Either T.Text (User, T.Text))
requestResetPassword email = do
  mu <- getUserLogin " " email
  case mu of
    Nothing ->
      pure (Left "A user with this email address does not exist.")

    Just (u, _) -> do
      r <- Sql.query u $
        Sql.statement
          "insert into lost_passwords values ($1, md5(random()::text), now() + '1 day') \
          \on conflict (user_id) do update set valid_until = EXCLUDED.valid_until returning hash"
          (contramap userId (SqlE.value SqlE.int4))
          (SqlD.singleRow $ SqlD.value SqlD.text)
          True
      pure $ pure (u, r)


verifyResetPassword :: T.Text -> T.Text -> BS.ByteString -> Sql.Transaction (Either T.Text User)
verifyResetPassword hash email newpass = do
  mu <- Sql.query (email, hash) $
    Sql.statement
      "select u.user_id, u.user_name, u.user_email, u.user_isadmin, u.user_wants_updates \
      \from lost_passwords l inner join users u on l.user_id = u.user_id \
      \where u.user_email = $1 and l.hash = $2 and l.valid_until > now()"
      (contramap fst (SqlE.value SqlE.text) <> contramap snd (SqlE.value SqlE.text))
      (SqlD.maybeRow decodeUser)
      True

  case mu of
    Nothing ->
      pure (Left "Could not find associated email. Maybe the request expired?")

    Just u -> do
      r <- Sql.query (userId u, newpass) $
        Sql.statement
          "update users set user_password_hash = $2 where user_id = $1 returning user_id, user_name, user_email, user_isadmin, user_wants_updates"
          (contramap fst (SqlE.value SqlE.int4) <> contramap snd (SqlE.value SqlE.bytea))
          (SqlD.singleRow decodeUser)
          True
      pure (pure r)

removeResetPassword :: UserId -> Sql.Transaction ()
removeResetPassword uid =
  Sql.query uid $
    Sql.statement
      "delete from lost_passwords where user_id = $1"
      (SqlE.value SqlE.int4)
      SqlD.unit
      True

cleanOldLostPasswords :: Sql.Transaction ()
cleanOldLostPasswords = Sql.query () $
  Sql.statement
    "delete from lost_passwords where valid_until < now()"
    mempty
    SqlD.unit
    True


--------------
-- Encoding --
--------------

-- | Encode a new User data type with password hash for inserts
-- | as a database row in the users table
-- |
-- | Warning: When changing this `newUser` should change as well
encodeNewUser :: SqlE.Params (User, BS.ByteString)
encodeNewUser =
    contramap (userName . fst) (SqlE.value SqlE.text)
 <> contramap (userEmail . fst) (SqlE.value SqlE.text)
 <> contramap (userIsAdmin . fst) (SqlE.value SqlE.bool)
 <> contramap (userWantsUpdates . fst) (SqlE.value SqlE.bool)
 <> contramap snd (SqlE.value SqlE.bytea)

-- | Encode an existing User data type for updates
-- | as a database row in the users table
-- |
-- | Warning: When changing this `updateUser` should change as well
encodeExistingUser :: SqlE.Params User
encodeExistingUser =
    contramap userId (SqlE.value SqlE.int4)
 <> contramap userName (SqlE.value SqlE.text)
 <> contramap userEmail (SqlE.value SqlE.text)
 <> contramap userIsAdmin (SqlE.value SqlE.bool)
 <> contramap userWantsUpdates (SqlE.value SqlE.bool)

-- | Encode an existing User data type with password hash for password updates
-- | as a database row in the users table
-- |
-- | Warning: When changing this `updateUser` should change as well
encodeExistingUserWithPassword :: SqlE.Params (User, BS.ByteString)
encodeExistingUserWithPassword =
    contramap (userId . fst) (SqlE.value SqlE.int4)
 <> contramap (userName . fst) (SqlE.value SqlE.text)
 <> contramap (userEmail . fst) (SqlE.value SqlE.text)
 <> contramap (userIsAdmin . fst) (SqlE.value SqlE.bool)
 <> contramap (userWantsUpdates . fst) (SqlE.value SqlE.bool)
 <> contramap snd (SqlE.value SqlE.bytea)


-- | Encode an Event data type for inserts as a database row in the events table
-- |
-- | Warning: When changing this `newEvent` should change as well
encodeNewEvent :: SqlE.Params Event
encodeNewEvent =
    contramap eventName (SqlE.value SqlE.text)
 <> contramap eventDesc (SqlE.value SqlE.text)
 <> contramap eventLocation (SqlE.value SqlE.text)
 <> contramap eventDateTime (SqlE.value SqlE.timestamptz)
 <> contramap eventDuration (SqlE.value SqlE.interval)

-- | Encode an Event data type for inserts as a database row in the events table
-- |
-- | Warning: When changing this `updateEvent` should change as well
encodeExistingEvent :: SqlE.Params Event
encodeExistingEvent =
    contramap eventId (SqlE.value SqlE.int4)
 <> contramap eventName (SqlE.value SqlE.text)
 <> contramap eventDesc (SqlE.value SqlE.text)
 <> contramap eventLocation (SqlE.value SqlE.text)
 <> contramap eventDateTime (SqlE.value SqlE.timestamptz)
 <> contramap eventDuration (SqlE.value SqlE.interval)


-- | Encode an (Event, Attendant) data type as a database row in the attendants table
-- |
-- | Warning: When changing this `upsertAttendant` should change as well
encodeAttendant :: SqlE.Params (Event, Attendant)
encodeAttendant =
    contramap (eventId . fst) (SqlE.value SqlE.int4)
 <> contramap (userId . attendantUser . snd) (SqlE.value SqlE.int4)
 <> contramap (attendantAttending . snd) (SqlE.value SqlE.bool)
 <> contramap (attendantFollowsChanges . snd) (SqlE.value SqlE.bool)

-- | Encode an (Event, Attendant) data type as a database row in the attendants table
-- |
-- | Warning: When changing this `removeAttendant` should change as well
encodeEventUserIds :: SqlE.Params (Event, User)
encodeEventUserIds =
    contramap (eventId . fst) (SqlE.value SqlE.int4)
 <> contramap (userId  . snd) (SqlE.value SqlE.int4)

--------------
-- Decoding --
--------------

-- | Decode a User data type as a row from the users table
decodeUser :: SqlD.Row User
decodeUser = User
  <$> SqlD.value SqlD.int4 -- id
  <*> SqlD.value SqlD.text -- name
  <*> SqlD.value SqlD.text -- email
  <*> SqlD.value SqlD.bool -- is admin
  <*> SqlD.value SqlD.bool -- wants updates

-- | Decode a UserSession data type as a row from the sessions table
decodeUserSession :: SqlD.Row UserSession
decodeUserSession = UserSession
  <$> SqlD.value SqlD.int4 -- user id
  <*> SqlD.value SqlD.timestamptz -- valid until

-- | Decode an Event data type as a row from the events table
decodeEvent :: SqlD.Row Event
decodeEvent = Event
  <$> SqlD.value SqlD.int4 -- id
  <*> SqlD.value SqlD.text -- name
  <*> SqlD.value SqlD.text -- description
  <*> SqlD.value SqlD.text -- location
  <*> SqlD.value SqlD.timestamptz -- time and date
  <*> SqlD.value SqlD.interval -- duration

-- | Decode attendant data as a row from the attendants table
decodeAttendant :: SqlD.Row (Int32, Int32, Bool, Bool)
decodeAttendant = (,,,)
  <$> SqlD.value SqlD.int4 -- event id
  <*> SqlD.value SqlD.int4 -- user id
  <*> SqlD.value SqlD.bool -- attending
  <*> SqlD.value SqlD.bool -- follows changes

-- | Decode an attendant data type as a row from the attendants table join with users table
decodeAttendantUser :: SqlD.Row Attendant
decodeAttendantUser = Attendant
  <$> decodeUser
  <*> SqlD.value SqlD.bool -- attending
  <*> SqlD.value SqlD.bool -- follows changes


-- | create an Attendant data type from events, users and attendants table data
toAttendant :: [Event] -> [User] -> (Int32, Int32, Bool, Bool) -> Maybe (Event, Attendant)
toAttendant events users (eid, uid, att, fol) =
  case (,)
    <$> find ((==) eid . eventId) events
    <*> find ((==) uid . userId) users
  of
    Just (e,u) -> pure (e, Attendant u att fol)
    Nothing -> Nothing

---------------------
-- Connection pool --
---------------------

hasqlPool :: Sql.Settings -> SC.ConnBuilder Sql.Connection
hasqlPool connstr = SC.ConnBuilder
  { cb_createConn = either (error . show . fmap BSC.unpack) id <$> Sql.acquire connstr
  , cb_destroyConn = Sql.release
  , cb_poolConfiguration = PoolCfg 10 1000 30
  }
