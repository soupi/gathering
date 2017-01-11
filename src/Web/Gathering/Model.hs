{- | Module describing the model for the application

In this application we want to be able to present users of a list of events
they can mark if they want to attend or not.

- A User is a definition of a user of the system
- An Event is an event presented by the admins of the website
- An attendant is a relation between users and events, describing
  whether a user is going to attend the event or nothing

- UserSession is for managing the session of a user on the website

-}

module Web.Gathering.Model where

import Data.Int (Int32)
import Data.Time (UTCTime, DiffTime)
import Data.Map.Strict (Map)
import Data.Text (Text)

type UserId = Int32

-- | Describing a user of the system
data User = User
  { userId :: Int32
  , userName :: Text
  , userEmail :: Text
  , userIsAdmin :: Bool
  , userWantsUpdates :: Bool
  }
  deriving (Show, Read, Eq, Ord)

-- | Describing a gathering event
data Event = Event
  { eventId :: Int32
  , eventName :: Text
  , eventDesc :: Text
  , eventLocation :: Text
  , eventDateTime :: UTCTime
  , eventDuration :: DiffTime
  }
  deriving (Show, Eq, Ord)

-- | Describing a user in the system that might attend an event
data Attendant = Attendant
  { attendantUser :: User
  , attendantAttending :: Bool
  , attendantFollowsChanges :: Bool
  }
  deriving (Show, Read, Eq, Ord)

-- | A mapping from events to attendants
type Attendants = Map Event [Attendant]


-- | Describing a user session
data UserSession = UserSession
  { usUserId :: Int32
  , usValidUntil :: UTCTime
  }
  deriving (Show, Eq, Ord)
