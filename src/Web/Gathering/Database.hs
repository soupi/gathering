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
import qualified Hasql.Session as Sql
import qualified Hasql.Decoders as SqlD
import qualified Hasql.Encoders as SqlE

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Functor.Contravariant (contramap)
import Data.Monoid
import Data.Int (Int32)
import Data.List (find)
import Data.Maybe (mapMaybe)

-----------
-- Query --
-----------

-- | Get a user from the users table with a specific id
getUserById :: UserId -> Sql.Session (Maybe User)
getUserById uid = Sql.query uid $
  Sql.statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates from users where user_id = $1"
    (SqlE.value SqlE.int4)
    (SqlD.maybeRow decodeUser)
    True

-- | Find a user by matching either their username or email
getUserLogin :: T.Text -> T.Text -> Sql.Session (Maybe (User, BS.ByteString))
getUserLogin name email = Sql.query (name, email) $
  Sql.statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates, user_password_hash from users where user_name = $1 OR user_email = $2"
    (  fst `contramap` SqlE.value SqlE.text
    <> snd `contramap` SqlE.value SqlE.text
    )
    (SqlD.maybeRow ((,) <$> decodeUser <*> SqlD.value SqlD.bytea))
    True

-- | Get a user from the users table from a session
getUserBySession :: UserId -> Sql.Session (Maybe User)
getUserBySession uid = Sql.query uid $
  Sql.statement
    "select users.user_id, users.user_name, users.user_email, users.user_isadmin, users.user_wants_updates from users join sessions on users.user_id = sessions.user_id and sessions.valid_until > now() and users.user_id = $1"
    (SqlE.value SqlE.int4)
    (SqlD.maybeRow decodeUser)
    True


-- | Get users from the users table
getUsers :: Sql.Session [User]
getUsers = Sql.query () $
  Sql.statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates from users"
    mempty
    (SqlD.rowsList decodeUser)
    False

-- | Get events from the events table
getEvents :: Sql.Session [Event]
getEvents = Sql.query () $
  Sql.statement
    "select * from events"
    mempty
    (SqlD.rowsList decodeEvent)
    False

-- | Get future events from the events table
getFutureEvents :: Sql.Session [Event]
getFutureEvents = Sql.query () $
  Sql.statement
    "select * from events where event_datetime >= now() order by event_datetime asc"
    mempty
    (SqlD.rowsList decodeEvent)
    False

-- | Get future events from the events table
getPastEvents :: Sql.Session [Event]
getPastEvents = Sql.query () $
  Sql.statement
    "select * from events where event_datetime < now() order by event_datetime desc"
    mempty
    (SqlD.rowsList decodeEvent)
    False

-- | Get all attendants
getAttendants :: Sql.Session Attendants
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
getAttendantsForEvent :: Event -> Sql.Session Attendants
getAttendantsForEvent event = do
  attendants <- Sql.query event $
    Sql.statement
      "select * from attendants where event_id = $1"
      (eventId `contramap` SqlE.value SqlE.int4)
      (SqlD.rowsList decodeAttendant)
      True
  users <- filter ((`elem` map snd4 attendants) . userId) <$> getUsers
  let atts' = mapMaybe (fmap (fmap (:[])) . toAttendant [event] users) attendants -- @TODO: Danger! ignoring bad values
  pure $
    M.unionsWith (<>) $ map (M.fromList . (:[])) atts'

-------------------
-- Insert/Update --
-------------------

-- | Add a new user to the users table

newUser :: User -> BS.ByteString -> Sql.Session User
newUser user pass = do
  Sql.query (user, pass) $
    Sql.statement
      "insert into users (user_name, user_email, user_isadmin, user_wants_updates, user_password_hash) values ($1, $2, $3, $4, $5)"
      encodeNewUser
      SqlD.unit
      True
  result <- getUserLogin (userName user) (userEmail user)
  maybe
    (fail "Internal error: Could not find user after insert.")
    (pure . fst)
    result

-- | Update an existing user in the users table
updateUser :: User -> Sql.Session User
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

-- | Update an existing user in the users table including the password
updateUserWithPassword :: User -> BS.ByteString -> Sql.Session User
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


-- | Add a new event to events table
newEvent :: Event -> Sql.Session ()
newEvent event = Sql.query event $
  Sql.statement
    "insert into events (event_name, event_description, event_location, event_datetime, event_duration) values ($1, $2, $3, $4, $5)"
    encodeNewEvent
    SqlD.unit
    True

-- | Add or update a user session in the sessions table. Set to expire after 1 month
upsertUserSession :: UserId -> Sql.Session ()
upsertUserSession uid = Sql.query uid $
  Sql.statement
    "insert into sessions values ($1, now() + '1 month') on conflict (user_id) do update set valid_until = EXCLUDED.valid_until"
    (SqlE.value SqlE.int4)
    SqlD.unit
    True

-- | Update an existing event in the events table
updateEvent :: Event -> Sql.Session ()
updateEvent event = Sql.query event $
  Sql.statement
    "update events set event_name = $2, event_description = $3, event_location = $4, event_datetime = $5, event_duration = $6 where event_id = $1"
    encodeExistingEvent
    SqlD.unit
    True

-- | Add or update an attendant for an event in the attendants table
upsertAttendant :: Event -> Attendant -> Sql.Session ()
upsertAttendant event att = Sql.query (event, att) $
  Sql.statement
    "insert into attendants values ($1, $2, $3, $4) on conflict (event_id, user_id) do update set attending = EXCLUDED.attending, follow_changes = EXCLUDED.follow_changes"
    encodeAttendant
    SqlD.unit
    True

-- | Kill a session for a user
killSession :: UserId -> Sql.Session ()
killSession uid = Sql.query uid $
  Sql.statement
    "delete from sessions where user_id = $1"
    (SqlE.value SqlE.int4)
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

-- | Decode an attendant data type as a row from the attendants table
decodeAttendant :: SqlD.Row (Int32, Int32, Bool, Bool)
decodeAttendant = (,,,)
  <$> SqlD.value SqlD.int4 -- event id
  <*> SqlD.value SqlD.int4 -- user id
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
