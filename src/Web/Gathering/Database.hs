{- | Module describing the database interaction

For functions that return `Transaction <result>`:

- If it only reads use `runReadTransaction` or `readQuery` in an Action context
- If it writes as well use `runWriteTransaction` or `writeQuery` in an Action context

Queries and Insert/Upsert queries are the interesting ones.

-}

module Web.Gathering.Database where

import Web.Gathering.Model
import Web.Gathering.Utils
import Web.Gathering.Types

import Web.Spock.Config
import qualified Web.Spock as S
import qualified Web.Spock.Config as SC
import qualified Hasql.Connection as Sql
import qualified Hasql.Session as Sql hiding (statement)
import qualified Hasql.Decoders as SqlD
import qualified Hasql.Encoders as SqlE
import qualified Hasql.Statement as Sql
import qualified Hasql.Transaction as Sql
import qualified Hasql.Transaction.Sessions as Sql

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Functor.Contravariant (contramap)
import Data.Functor (($>))
import Data.Int (Int32)
import Data.List (find)
import Data.Maybe (mapMaybe)


-- | Run a transaction that only reads data from the database
--   in the context of a spock action
readQuery :: Sql.Transaction a -> Action v (Either Sql.QueryError a)
readQuery = S.runQuery . Sql.run . runReadTransaction

-- | Run a transaction that can read and write data from/to the database
--   in the context of a spock action
writeQuery :: Sql.Transaction a -> Action v (Either Sql.QueryError a)
writeQuery = S.runQuery . Sql.run . runWriteTransaction

-- | Run a transaction that only reads data from the database
runReadTransaction :: Sql.Transaction a -> Sql.Session a
runReadTransaction = Sql.transaction Sql.Serializable Sql.Read

-- | Run a transaction that can read and write data from/to the database
runWriteTransaction :: Sql.Transaction a -> Sql.Session a
runWriteTransaction = Sql.transaction Sql.Serializable Sql.Write

-----------
-- Query --
-----------

-- | Retrieve a user from the new_users table. Will have the verification random in the userId field.
--   Will return Nothing if the user does not exist
getNewUser :: T.Text -> T.Text -> Sql.Transaction (Maybe User)
getNewUser name email =
  Sql.statement (name, email) $
    Sql.Statement
      "select verification_rand, user_name, user_email, user_isadmin, user_wants_updates, '' from new_users where (user_name = $1 or user_email = $2) and valid_until > now()"
      (contramap fst (param SqlE.text) <> contramap snd (param SqlE.text))
      (SqlD.rowMaybe decodeUser)
      True

-- | Get a user from the users table with a specific id
getUserById :: UserId -> Sql.Transaction (Maybe User)
getUserById uid = Sql.statement uid $
  Sql.Statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates, user_hash from users where user_id = $1"
    (param SqlE.int4)
    (SqlD.rowMaybe decodeUser)
    True

-- | Find a user by matching either their username or email
getUserLogin :: T.Text -> T.Text -> Sql.Transaction (Maybe (User, BS.ByteString))
getUserLogin name email =
  Sql.statement (name, email) $
    Sql.Statement
      "select user_id, user_name, user_email, user_isadmin, user_wants_updates, user_hash, user_password_hash from users where user_name = $1 OR user_email = $2"
      (fst `contramap` param SqlE.text <> snd `contramap` param SqlE.text)
      (SqlD.rowMaybe ((,) <$> decodeUser <*> column SqlD.bytea))
      True

-- | Get a user from the users table from a session
getUserBySession :: UserId -> Sql.Transaction (Maybe User)
getUserBySession uid =
  Sql.statement uid $
    Sql.Statement
      "select users.user_id, users.user_name, users.user_email, users.user_isadmin, users.user_wants_updates, user_hash from users join sessions on users.user_id = sessions.user_id and sessions.valid_until > now() and users.user_id = $1"
      (param SqlE.int4)
      (SqlD.rowMaybe decodeUser)
      True

-- | Find a user by email and hash
getUserEmailHash :: T.Text -> T.Text -> Sql.Transaction (Maybe User)
getUserEmailHash email hash = Sql.statement (email, hash) $
  Sql.Statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates, user_hash, user_password_hash from users where user_name = $1 OR user_email = $2"
    (  fst `contramap` param SqlE.text
    <> snd `contramap` param SqlE.text
    )
    (SqlD.rowMaybe decodeUser)
    True

-- | Get users from the users table
getUsers :: Sql.Transaction [User]
getUsers = Sql.statement () $
  Sql.Statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates, user_hash from users"
    mempty
    (SqlD.rowList decodeUser)
    False

-- | Get events from the events table
getEvents :: Sql.Transaction [Event]
getEvents = Sql.statement () $
  Sql.Statement
    "select * from events"
    mempty
    (SqlD.rowList decodeEvent)
    False

-- | Get future events from the events table
getFutureEvents :: Sql.Transaction [Event]
getFutureEvents = Sql.statement () $
  Sql.Statement
    "select * from events where event_datetime + event_duration >= now() order by event_datetime asc"
    mempty
    (SqlD.rowList decodeEvent)
    False

-- | Get future events from the events table
getPastEvents :: Sql.Transaction [Event]
getPastEvents = Sql.statement () $
  Sql.Statement
    "select * from events where event_datetime + event_duration < now() order by event_datetime desc"
    mempty
    (SqlD.rowList decodeEvent)
    False

-- | Get an event by id from the events table
getEventById :: Int32 -> Sql.Transaction (Maybe Event)
getEventById eid = Sql.statement eid $
  Sql.Statement
    "select * from events where event_id = $1"
    (param SqlE.int4)
    (SqlD.rowMaybe decodeEvent)
    False

getNewEvents :: Sql.Transaction [((Event, Bool), [Attendant])]
getNewEvents = do
  events <- Sql.statement () $
    Sql.Statement
      "select e.*, en.is_edit from events e inner join new_events en on e.event_id = en.event_id where send_after < now()"
      mempty
      (SqlD.rowList $ (,) <$> decodeEvent <*> column SqlD.bool)
      False

  results <- mapM (\e -> (e,) <$> getAttendantsForEvent (fst e)) events

  pure results


getNearEvents :: Sql.Transaction [(Event, [Attendant])]
getNearEvents = do
  events <- Sql.statement () $
    Sql.Statement
      "select * from events where event_datetime between (now() + '19 hours') and (now() + '24 hours') or event_datetime between (now()) and (now() + '5 hours')"
      mempty
      (SqlD.rowList decodeEvent)
      False

  results <- mapM (\e -> (e,) <$> getAttendantsForEvent e) events

  pure results


-- | Get all attendants
getAttendants :: Sql.Transaction Attendants
getAttendants = do
  attendants <- Sql.statement () $
    Sql.Statement
      "select * from attendants"
      mempty
      (SqlD.rowList decodeAttendant)
      False
  events <- filter ((`elem` map fst4 attendants) . eventId) <$> getEvents
  users <- filter ((`elem` map snd4 attendants) . userId) <$> getUsers
  let atts' = mapMaybe (fmap (fmap (:[])) . toAttendant events users) attendants -- @TODO: Danger! ignoring bad values
  pure $
    M.unionsWith (<>) $ map (M.fromList . (:[])) atts'


-- | Get attendants for an event
getAttendantsForEvent :: Event -> Sql.Transaction [Attendant]
getAttendantsForEvent event = do
  Sql.statement event $
    Sql.Statement
      "select u.user_id, u.user_name, u.user_email, u.user_isadmin, u.user_wants_updates, u.user_hash, a.attending, a.follow_changes from attendants a inner join users u on a.user_id = u.user_id where a.event_id = $1"
      (eventId `contramap` param SqlE.int4)
      (SqlD.rowList decodeAttendantUser)
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
          Sql.statement (user, pass) $
            Sql.Statement
              "insert into new_users (verification_rand, user_name, user_email, user_isadmin, user_wants_updates, user_password_hash, valid_until) values (round(random() * 2000000000), $1, $2, $3, $4, $5, now() + '2 days')"
              encodeNewUser
              SqlD.noResult
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
      insertResult <- Sql.statement (key, email) $
        Sql.Statement
          "insert into users (user_name, user_email, user_isadmin, user_wants_updates, user_password_hash, user_hash) select user_name, user_email, user_isadmin, user_wants_updates, user_password_hash, md5(random()::text) from new_users where verification_rand = $1 and user_email = $2 and valid_until > now() returning user_id, user_name, user_email, user_isadmin, user_wants_updates, user_hash"
          (contramap fst (param SqlE.int4) <> contramap snd (param SqlE.text))
          (SqlD.rowMaybe decodeUser)
          True

      case insertResult of
        Just r ->
          pure (pure r)
        Nothing ->
          pure (Left "Could not find associated email. Maybe verification expired?")


-- | Update an existing user in the users table
updateUser :: User -> Sql.Transaction (Either T.Text User)
updateUser user = do
  Sql.statement user $
    Sql.Statement
      "update users set user_name = $2, user_email = $3, user_isadmin = $4, user_wants_updates = $5 where user_id = $1"
      encodeExistingUser
      SqlD.noResult
      True
  result <- getUserLogin (userName user) (userEmail user)
  maybe
    (pure $ Left "Internal error: Could not find user after update.")
    (pure . Right . fst)
    result

-- | Give admin to user by name or email
changeAdminForUser :: Bool -> T.Text -> Sql.Transaction (Either T.Text ())
changeAdminForUser isAdmin login = do
  mUser <- getUserLogin login login
  case mUser of
    Nothing ->
      pure $ Left "User not found."
    Just (user,_) ->
      updateUser (user { userIsAdmin = isAdmin }) $> pure ()

-- | Delete user by name or email from system
deleteUser :: T.Text -> Sql.Transaction (Either T.Text ())
deleteUser login = do
  mUser <- getUserLogin login login
  case mUser of
    Nothing ->
      pure $ Left "User not found."
    Just (user,_) -> do
      Sql.statement (userId user) $
        Sql.Statement
          "delete from attendants where user_id = $1"
          (param SqlE.int4)
          SqlD.noResult
          True

      Sql.statement (userId user) $
        Sql.Statement
          "delete from users where user_id = $1"
          (param SqlE.int4)
          SqlD.noResult
          True

      pure $ pure ()

-- | Update an existing user in the users table including the password
updateUserWithPassword :: User -> BS.ByteString -> Sql.Transaction (Either T.Text User)
updateUserWithPassword user pass = do
  Sql.statement (user, pass) $
    Sql.Statement
      "update users set user_name = $2, user_email = $3, user_isadmin = $4, user_wants_updates = $5, user_password_hash = $6 where user_id = $1"
      encodeExistingUserWithPassword
      SqlD.noResult
      True
  result <- getUserLogin (userName user) (userEmail user)
  maybe
    (pure $ Left "Internal error: Could not find user after update.")
    (pure . Right. fst)
    result

-- | Delete a user from the new_users table based on their email and verification_rand
removeNewUser :: User -> Sql.Transaction ()
removeNewUser user = do
  Sql.statement (userId user, userEmail user) $
    Sql.Statement
      "delete from new_users where verification_rand = $1 and user_email = $2"
      (contramap fst (param SqlE.int4) <> contramap snd (param SqlE.text))
      SqlD.noResult
      True

-- Event --

-- | Add a new event to events table
newEvent :: Event -> Sql.Transaction EventId
newEvent event = do
  eid <- Sql.statement event $
    Sql.Statement
      "insert into events (event_name, event_description, event_location, event_datetime, event_duration) values ($1, $2, $3, $4, $5) returning event_id"
      encodeNewEvent
      (SqlD.singleRow $ column SqlD.int4)
      True

  Sql.statement eid $
    Sql.Statement
      "insert into new_events values ($1, false, now() + '15 minutes')"
      (param SqlE.int4)
      SqlD.noResult
      True

  pure eid


-- | Update an existing event in the events table
updateEvent :: Event -> Sql.Transaction ()
updateEvent event = do
  Sql.statement event $
    Sql.Statement
      "update events set event_name = $2, event_description = $3, event_location = $4, event_datetime = $5, event_duration = $6 where event_id = $1"
      encodeExistingEvent
      SqlD.noResult
      True

  Sql.statement (eventId event) $
    Sql.Statement
      "insert into new_events values ($1, true, now() + '15 minutes') on conflict (event_id) do update set is_edit = EXCLUDED.is_edit, send_after = EXCLUDED.send_after"
      (param SqlE.int4)
      SqlD.noResult
      True

-- | Remove an existing event from the events table
removeEvent :: Event -> Sql.Transaction ()
removeEvent event = do
  removeAttendants event
  removeNewEvent event
  Sql.statement (eventId event) $
    Sql.Statement
      "delete from events where event_id = $1"
      (param SqlE.int4)
      SqlD.noResult
      True

removeNewEvent :: Event -> Sql.Transaction ()
removeNewEvent event =
  Sql.statement (eventId event) $
    Sql.Statement
      "delete from new_events where event_id = $1"
      (param SqlE.int4)
      SqlD.noResult
      True


-- Attendant --

-- | Add or update an attendant for an event in the attendants table
upsertAttendant :: Event -> Attendant -> Sql.Transaction ()
upsertAttendant event att = Sql.statement (event, att) $
  Sql.Statement
    "insert into attendants values ($1, $2, $3, $4) on conflict (event_id, user_id) do update set attending = EXCLUDED.attending, follow_changes = EXCLUDED.follow_changes"
    encodeAttendant
    SqlD.noResult
    True

-- | Remove an attendant for an event from the attendants table
removeAttendant :: Event -> User -> Sql.Transaction ()
removeAttendant event user = Sql.statement (event, user) $
  Sql.Statement
    "delete from attendants where event_id = $1 and user_id = $2"
    encodeEventUserIds
    SqlD.noResult
    True

-- | Remove all attendants for an event from the attendants table
removeAttendants :: Event -> Sql.Transaction ()
removeAttendants event = Sql.statement (eventId event) $
  Sql.Statement
    "delete from attendants where event_id = $1"
    (param SqlE.int4)
    SqlD.noResult
    True

-- UserSession --

-- | Add or update a user session in the sessions table. Set to expire after 1 month
upsertUserSession :: UserId -> Sql.Transaction ()
upsertUserSession uid = Sql.statement uid $
  Sql.Statement
    "insert into sessions values ($1, now() + '1 month') on conflict (user_id) do update set valid_until = EXCLUDED.valid_until"
    (param SqlE.int4)
    SqlD.noResult
    True


-- | Kill a session for a user
killSession :: UserId -> Sql.Transaction ()
killSession uid = Sql.statement uid $
  Sql.Statement
    "delete from sessions where user_id = $1"
    (param SqlE.int4)
    SqlD.noResult
    True

-- | Delete sessions which have expired
cleanOldSessions :: Sql.Transaction ()
cleanOldSessions = Sql.statement () $
  Sql.Statement
    "delete from sessions where valid_until < now()"
    mempty
    SqlD.noResult
    True

cleanOldNewUsers :: Sql.Transaction ()
cleanOldNewUsers = Sql.statement () $
  Sql.Statement
    "delete from new_users where valid_until < now()"
    mempty
    SqlD.noResult
    True

-- Lost passwords --

requestResetPassword :: T.Text -> Sql.Transaction (Either T.Text (User, T.Text))
requestResetPassword email = do
  mu <- getUserLogin " " email
  case mu of
    Nothing ->
      pure (Left "A user with this email address does not exist.")

    Just (u, _) -> do
      r <- Sql.statement u $
        Sql.Statement 
          "insert into lost_passwords values ($1, md5(random()::text), now() + '1 day') \
          \on conflict (user_id) do update set valid_until = EXCLUDED.valid_until returning hash"
          (contramap userId (param SqlE.int4))
          (SqlD.singleRow $ column SqlD.text)
          True
      pure $ pure (u, r)


verifyResetPassword :: T.Text -> T.Text -> BS.ByteString -> Sql.Transaction (Either T.Text User)
verifyResetPassword hash email newpass = do
  mu <- Sql.statement (email, hash) $
    Sql.Statement 
      "select u.user_id, u.user_name, u.user_email, u.user_isadmin, u.user_wants_updates, u.user_hash \
      \from lost_passwords l inner join users u on l.user_id = u.user_id \
      \where u.user_email = $1 and l.hash = $2 and l.valid_until > now()"
      (contramap fst (param SqlE.text) <> contramap snd (param SqlE.text))
      (SqlD.rowMaybe decodeUser)
      True

  case mu of
    Nothing ->
      pure (Left "Could not find associated email. Maybe the request expired?")

    Just u -> do
      r <- Sql.statement (userId u, newpass) $
        Sql.Statement 
          "update users set user_password_hash = $2 where user_id = $1 returning user_id, user_name, user_email, user_isadmin, user_wants_updates, user_hash"
          (contramap fst (param SqlE.int4) <> contramap snd (param SqlE.bytea))
          (SqlD.singleRow decodeUser)
          True
      pure (pure r)

removeResetPassword :: UserId -> Sql.Transaction ()
removeResetPassword uid =
  Sql.statement uid $
    Sql.Statement
      "delete from lost_passwords where user_id = $1"
      (param SqlE.int4)
      SqlD.noResult
      True

cleanOldLostPasswords :: Sql.Transaction ()
cleanOldLostPasswords = Sql.statement () $
  Sql.Statement
    "delete from lost_passwords where valid_until < now()"
    mempty
    SqlD.noResult
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
    contramap (userName . fst) (param SqlE.text)
 <> contramap (userEmail . fst) (param SqlE.text)
 <> contramap (userIsAdmin . fst) (param SqlE.bool)
 <> contramap (userWantsUpdates . fst) (param SqlE.bool)
 <> contramap snd (param SqlE.bytea)

-- | Encode an existing User data type for updates
-- | as a database row in the users table
-- |
-- | Warning: When changing this `updateUser` should change as well
encodeExistingUser :: SqlE.Params User
encodeExistingUser =
    contramap userId (param SqlE.int4)
 <> contramap userName (param SqlE.text)
 <> contramap userEmail (param SqlE.text)
 <> contramap userIsAdmin (param SqlE.bool)
 <> contramap userWantsUpdates (param SqlE.bool)

-- | Encode an existing User data type with password hash for password updates
-- | as a database row in the users table
-- |
-- | Warning: When changing this `updateUser` should change as well
encodeExistingUserWithPassword :: SqlE.Params (User, BS.ByteString)
encodeExistingUserWithPassword =
    contramap (userId . fst) (param SqlE.int4)
 <> contramap (userName . fst) (param SqlE.text)
 <> contramap (userEmail . fst) (param SqlE.text)
 <> contramap (userIsAdmin . fst) (param SqlE.bool)
 <> contramap (userWantsUpdates . fst) (param SqlE.bool)
 <> contramap snd (param SqlE.bytea)


-- | Encode an Event data type for inserts as a database row in the events table
-- |
-- | Warning: When changing this `newEvent` should change as well
encodeNewEvent :: SqlE.Params Event
encodeNewEvent =
    contramap eventName (param SqlE.text)
 <> contramap eventDesc (param SqlE.text)
 <> contramap eventLocation (param SqlE.text)
 <> contramap eventDateTime (param SqlE.timestamptz)
 <> contramap eventDuration (param SqlE.interval)

-- | Encode an Event data type for inserts as a database row in the events table
-- |
-- | Warning: When changing this `updateEvent` should change as well
encodeExistingEvent :: SqlE.Params Event
encodeExistingEvent =
    contramap eventId (param SqlE.int4)
 <> contramap eventName (param SqlE.text)
 <> contramap eventDesc (param SqlE.text)
 <> contramap eventLocation (param SqlE.text)
 <> contramap eventDateTime (param SqlE.timestamptz)
 <> contramap eventDuration (param SqlE.interval)


-- | Encode an (Event, Attendant) data type as a database row in the attendants table
-- |
-- | Warning: When changing this `upsertAttendant` should change as well
encodeAttendant :: SqlE.Params (Event, Attendant)
encodeAttendant =
    contramap (eventId . fst) (param SqlE.int4)
 <> contramap (userId . attendantUser . snd) (param SqlE.int4)
 <> contramap (attendantAttending . snd) (param SqlE.bool)
 <> contramap (attendantFollowsChanges . snd) (param SqlE.bool)

-- | Encode an (Event, Attendant) data type as a database row in the attendants table
-- |
-- | Warning: When changing this `removeAttendant` should change as well
encodeEventUserIds :: SqlE.Params (Event, User)
encodeEventUserIds =
    contramap (eventId . fst) (param SqlE.int4)
 <> contramap (userId  . snd) (param SqlE.int4)

--------------
-- Decoding --
--------------

-- | Decode a User data type as a row from the users table
decodeUser :: SqlD.Row User
decodeUser = User
  <$> column SqlD.int4 -- id
  <*> column SqlD.text -- name
  <*> column SqlD.text -- email
  <*> column SqlD.bool -- is admin
  <*> column SqlD.bool -- wants updates
  <*> column SqlD.text -- hash

-- | Decode a UserSession data type as a row from the sessions table
decodeUserSession :: SqlD.Row UserSession
decodeUserSession = UserSession
  <$> column SqlD.int4 -- user id
  <*> column SqlD.timestamptz -- valid until

-- | Decode an Event data type as a row from the events table
decodeEvent :: SqlD.Row Event
decodeEvent = Event
  <$> column SqlD.int4 -- id
  <*> column SqlD.text -- name
  <*> column SqlD.text -- description
  <*> column SqlD.text -- location
  <*> column SqlD.timestamptz -- time and date
  <*> column SqlD.interval -- duration

-- | Decode attendant data as a row from the attendants table
decodeAttendant :: SqlD.Row (Int32, Int32, Bool, Bool)
decodeAttendant = (,,,)
  <$> column SqlD.int4 -- event id
  <*> column SqlD.int4 -- user id
  <*> column SqlD.bool -- attending
  <*> column SqlD.bool -- follows changes

-- | Decode an attendant data type as a row from the attendants table join with users table
decodeAttendantUser :: SqlD.Row Attendant
decodeAttendantUser = Attendant
  <$> decodeUser
  <*> column SqlD.bool -- attending
  <*> column SqlD.bool -- follows changes


-- | create an Attendant data type from events, users and attendants table data
toAttendant :: [Event] -> [User] -> (Int32, Int32, Bool, Bool) -> Maybe (Event, Attendant)
toAttendant events users (eid, uid, att, fol) =
  case (,)
    <$> find ((==) eid . eventId) events
    <*> find ((==) uid . userId) users
  of
    Just (e,u) -> pure (e, Attendant u att fol)
    Nothing -> Nothing

--------------------------------
-- Encoding / Decoding helers --
--------------------------------
-- | Encode a NON-nullable value
param :: SqlE.Value a -> SqlE.Params a
param = SqlE.param . SqlE.nonNullable

-- | Decode a NON-nullable value
column :: SqlD.Value a -> SqlD.Row a
column = SqlD.column . SqlD.nonNullable

---------------------
-- Connection pool --
---------------------

-- | A connection pool for hasql
--   The numbers are fairly random. Sorry about that.
hasqlPool :: Sql.Settings -> SC.ConnBuilder Sql.Connection
hasqlPool connstr = SC.ConnBuilder
  { cb_createConn = either (error . show . fmap BSC.unpack) id <$> Sql.acquire connstr
  , cb_destroyConn = Sql.release
  , cb_poolConfiguration = PoolCfg 10 1000 30
  }
