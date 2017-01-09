{- | Module describing the database interaction
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
import Data.Maybe (mapMaybe, listToMaybe)

-----------
-- Query --
-----------

-- | Get a user from the users table with a specific id
getUserById :: UserId -> Sql.Session (Maybe User)
getUserById uid = Sql.query () $
  Sql.statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates from users where user_id = $1"
    (const uid `contramap` SqlE.value SqlE.int4)
    (SqlD.maybeRow decodeUser)
    True

-- | Find a user by matching either their username or email with a String and matching their password
getUserWithPass :: String -> String -> Sql.Session (Maybe User)
getUserWithPass (T.pack -> login) (BSC.pack -> pass) = Sql.query () $
  Sql.statement
    "select user_id, user_name, user_email, user_isadmin, user_wants_updates from users where (user_name = $1 OR user_email = $2) AND user_password_hash = $3"
    (  const login `contramap` SqlE.value SqlE.text
    <> const login `contramap` SqlE.value SqlE.text
    <> const pass  `contramap` SqlE.value SqlE.bytea
    )
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
  attendants <- Sql.query () $
    Sql.statement
      "select * from attendants where event_id = $1"
      (const (eventId event) `contramap` SqlE.value SqlE.int4)
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
newUser :: Sql.Query (User, BS.ByteString, BS.ByteString) ()
newUser =
  Sql.statement
    "insert into users (user_name, user_email, user_isadmin, user_wants_updates, user_password_hash, user_password_salt) values ($1, $2, $3, $4, $5, $6)"
    encodeNewUser
    SqlD.unit
    True

-- | Update an existing user in the users table
updateUser :: Sql.Query User ()
updateUser =
  Sql.statement
    "update users set user_name = $2, user_email = $3, user_isadmin = $4, user_wants_updates = $5 where user_id = $1"
    encodeExistingUser
    SqlD.unit
    True

-- | Update an existing user in the users table including the password
updateUserWithPassword :: Sql.Query (User, BS.ByteString, BS.ByteString) ()
updateUserWithPassword =
  Sql.statement
    "update users set user_name = $2, user_email = $3, user_isadmin = $4, user_wants_updates = $5, user_password_hash = $6, user_password_salt = $7 where user_id = $1"
    encodeExistingUserWithPassword
    SqlD.unit
    True


-- | Add a new event to events table
newEvent :: Sql.Query Event ()
newEvent =
  Sql.statement
    "insert into events (event_name, event_description, event_location, event_datetime, event_duration) values ($1, $2, $3, $4, $5)"
    encodeNewEvent
    SqlD.unit
    True

-- | Update an existing event in the events table
updateEvent :: Sql.Query Event ()
updateEvent =
  Sql.statement
    "update events set event_name = $2, event_description = $3, event_location = $4, event_datetime = $5, event_duration = $6 where event_id = $1"
    encodeExistingEvent
    SqlD.unit
    True

-- | Add or update an attendant for an event in the attendants table
upsertAttendant :: Sql.Query (Event, Attendant) ()
upsertAttendant =
  Sql.statement
    "insert into users values ($1, $2, $3, $4) on conflict (event_id, user_id) do update set attending = EXCLUDED.attending, follow_changes = EXCLUDED.follow_changes"
    encodeAttendant
    SqlD.unit
    True

--------------
-- Encoding --
--------------

-- | Encode a new User data type with password hash and salt for inserts
-- | as a database row in the users table
-- |
-- | Warning: When changing this `newUser` should change as well
encodeNewUser :: SqlE.Params (User, BS.ByteString, BS.ByteString)
encodeNewUser =
    contramap (userId . fst3) (SqlE.value SqlE.int4)
 <> contramap (T.pack . userName . fst3) (SqlE.value SqlE.text)
 <> contramap (T.pack . userEmail . fst3) (SqlE.value SqlE.text)
 <> contramap (userIsAdmin . fst3) (SqlE.value SqlE.bool)
 <> contramap (userWantsUpdates . fst3) (SqlE.value SqlE.bool)
 <> contramap snd3 (SqlE.value SqlE.bytea)
 <> contramap trd3 (SqlE.value SqlE.bytea)

-- | Encode an existing User data type for updates
-- | as a database row in the users table
-- |
-- | Warning: When changing this `updateUser` should change as well
encodeExistingUser :: SqlE.Params User
encodeExistingUser =
    contramap userId (SqlE.value SqlE.int4)
 <> contramap (T.pack . userName) (SqlE.value SqlE.text)
 <> contramap (T.pack . userEmail) (SqlE.value SqlE.text)
 <> contramap userIsAdmin (SqlE.value SqlE.bool)
 <> contramap userWantsUpdates (SqlE.value SqlE.bool)

-- | Encode an existing User data type with password hash and salt for password updates
-- | as a database row in the users table
-- |
-- | Warning: When changing this `updateUser` should change as well
encodeExistingUserWithPassword :: SqlE.Params (User, BS.ByteString, BS.ByteString)
encodeExistingUserWithPassword =
    contramap (userId . fst3) (SqlE.value SqlE.int4)
 <> contramap (T.pack . userName . fst3) (SqlE.value SqlE.text)
 <> contramap (T.pack . userEmail . fst3) (SqlE.value SqlE.text)
 <> contramap (userIsAdmin . fst3) (SqlE.value SqlE.bool)
 <> contramap (userWantsUpdates . fst3) (SqlE.value SqlE.bool)
 <> contramap snd3 (SqlE.value SqlE.bytea)
 <> contramap trd3 (SqlE.value SqlE.bytea)



-- | Encode an Event data type for inserts as a database row in the events table
-- |
-- | Warning: When changing this `newEvent` should change as well
encodeNewEvent :: SqlE.Params Event
encodeNewEvent =
    contramap (T.pack . eventName) (SqlE.value SqlE.text)
 <> contramap (T.pack . eventDesc) (SqlE.value SqlE.text)
 <> contramap (T.pack . eventLocation) (SqlE.value SqlE.text)
 <> contramap eventDateTime (SqlE.value SqlE.timestamptz)
 <> contramap eventDuration (SqlE.value SqlE.interval)

-- | Encode an Event data type for inserts as a database row in the events table
-- |
-- | Warning: When changing this `updateEvent` should change as well
encodeExistingEvent :: SqlE.Params Event
encodeExistingEvent =
    contramap eventId (SqlE.value SqlE.int4)
 <> contramap (T.pack . eventName) (SqlE.value SqlE.text)
 <> contramap (T.pack . eventDesc) (SqlE.value SqlE.text)
 <> contramap (T.pack . eventLocation) (SqlE.value SqlE.text)
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
  <*> fmap T.unpack (SqlD.value SqlD.text) -- name
  <*> fmap T.unpack (SqlD.value SqlD.text) -- email
  <*> SqlD.value SqlD.bool -- is admin
  <*> SqlD.value SqlD.bool -- wants updates

-- | Decode an Event data type as a row from the events table
decodeEvent :: SqlD.Row Event
decodeEvent = Event
  <$> SqlD.value SqlD.int4 -- id
  <*> fmap T.unpack (SqlD.value SqlD.text) -- name
  <*> fmap T.unpack (SqlD.value SqlD.text) -- description
  <*> fmap T.unpack (SqlD.value SqlD.text) -- location
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
