module Web.Gathering.Types
  ( module Web.Gathering.Model
  , module Web.Gathering.Types
  )
where

import Web.Gathering.Model

import Web.Spock
import Web.Spock.Config

import Data.IORef



data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

data AppCfg = AppCfg
  { cfgDB :: String
  , cfgPort :: Int
  , cfgTitle :: String
  , cfgDesc :: String
  }
  deriving (Show, Eq, Ord)




data IsAdmin = IsAdmin
  deriving (Show, Eq, Ord)

