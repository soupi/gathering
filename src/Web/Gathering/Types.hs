{- | Defining a few types for the application:

- The Session type (MySession) - a user id
- The AppState which in our case will just serve as the configuration for the website and will not change
- The DB connection, in our case will be a Postgresql connection

-}

module Web.Gathering.Types
  ( module Web.Gathering.Model
  , module Web.Gathering.Types
  )
where

import Web.Gathering.Model
import Web.Gathering.Config
import Web.Gathering.Workers.Logger

import Data.Int (Int32)
import Web.Spock
import Hasql.Connection (Connection)
import Control.Concurrent.STM (TQueue)

-- | Our session is empty when the user is not connected
--   And when they are connected it will be their user id
data MySession
  = EmptySession
  | SessionId Int32
  deriving (Show, Eq, Ord)

-- | The state is the app configuration and won't change
data AppState = AppState
  { appConfig  :: AppConfig
  , appCommand :: Command
  , appLogger  :: TQueue Msg
  }
  deriving (Eq)

-- | Convenient context type for Spock
type App ctx = SpockCtxM ctx Connection MySession AppState ()

-- | Convenient action type for Spock
type Action ctx a = SpockActionCtx ctx Connection MySession AppState a
