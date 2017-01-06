module Web.Gathering.Types
  ( module Web.Gathering.Model
  , module Web.Gathering.Types
  )
where

import Data.Int (Int32)
import Web.Gathering.Model
import Web.Gathering.Config

import Web.Spock

import Hasql.Connection (Connection)


data MySession
  = EmptySession
  | SessionId Int32
  deriving (Show, Eq, Ord)

data AppState = AppState
  { appConfig :: AppConfig
  }
  deriving (Show, Eq, Ord)


type App ctx = SpockCtxM ctx Connection MySession AppState ()
type Action ctx a = SpockActionCtx ctx Connection MySession AppState a
