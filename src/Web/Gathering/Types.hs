module Web.Gathering.Types
  ( module Web.Gathering.Model
  , module Web.Gathering.Types
  )
where

import Web.Gathering.Model
import Web.Gathering.Config

import Web.Spock
import Web.Spock.Config

data MySession = EmptySession

data AppState = AppState
  { appConfig :: AppConfig
  }
  deriving (Show, Eq, Ord)

data IsAdmin = IsAdmin
  deriving (Show, Eq, Ord)

