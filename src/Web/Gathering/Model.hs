{- | Module describing the model for the application
-}

module Web.Gathering.Model where

import Data.Int (Int32)
import Data.Time (UTCTime, DiffTime)
import Data.Map.Strict (Map)


-- | Describing a user of the system
data User = User
  { userId :: Int32
  , userName :: String
  , userEmail :: String
  , userIsAdmin :: Bool
  , userWantsUpdates :: Bool
  }
  deriving (Show, Read, Eq, Ord)

-- | Describing a gathering event
data Event = Event
  { eventId :: Int32
  , eventName :: String
  , eventDesc :: String
  , eventLocation :: String
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
