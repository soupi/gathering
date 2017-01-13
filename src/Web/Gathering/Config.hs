{- | Handle the configuration of the app

We'll use the functions defined in this module to parse
command line arguments and config files to provide the configuration
for this program, the configuration contains:

- Command: how to run the app: http, https or both
- AppConfig: configuration for the app which contains
    - Title
    - Description
    - DB connection string

Use `parseArgs` at the start of the program to get the configuration.
from the command line arguments

The defaults are defined in defaultConfig

-}

module Web.Gathering.Config
  ( parseArgs
  , parseConfig
  , Config(..)
  , AppConfig(..)
  , Command(..)
  , TLSConfig(..)
  , defaultConfig
  )
where

import Options.Applicative
import qualified Data.Text as T
import qualified Data.Configurator as C

-- | Will parse the arguments to the program and will produce
--   A configuration to run spock
--
--   May throw an error on bad configuration file or arguments
--
--   This is all you need from this module basically
parseArgs :: IO (AppConfig, Command)
parseArgs = do
  args <- execParser paramsParserInfo
  case (parseConfig <$> pFileCfg args, pCfg args, pCmd args) of
    (Just _, Just cfg, Just cmd') ->
      pure (cfg, cmd')

    (Just fc, Just cfg, Nothing) -> do
      (_, cmd') <- fc
      pure (cfg, cmd')

    (Just fc, Nothing, Nothing) -> do
      (cfg, cmd') <- fc
      pure (cfg, cmd')

    _ ->
      pure (defaultConfig, HTTP 8080)

-- | Parse configuration file
parseConfig :: FilePath -> IO (AppConfig, Command)
parseConfig cfgFile = do
  cfg <- C.load [C.Required cfgFile]
  name <- C.require cfg "blogName"
  desc <- C.require cfg "blogDescription"
  db <- C.require cfg "db"

  port <- C.lookup cfg "http.port"
  tlsport <- C.lookup cfg "https.port"
  tlscert <- C.lookup cfg "https.cert"
  tlskey  <- C.lookup cfg "https.key"

  cmd' <- case (port, (,,) <$> tlsport <*> tlscert <*> tlskey) of
    (Just port', Just (p,c,k)) ->
      pure $ Both port' (TLSConfig p c k)
    (Nothing, Just (p,c,k)) ->
      pure $ HTTPS (TLSConfig p c k)
    (Just port', _) ->
      pure $ HTTP port'
    _ ->
      error "http or https configuration missing from configuration file."

  pure (AppConfig db name desc, cmd')


------------
-- Config --
------------

-- | Configuration to run the website
data Config = Config
  { cConfig :: AppConfig
  , cCmd :: Command
  }
  deriving (Show)

-- | Application Configuration
data AppConfig = AppConfig
  { cfgTitle :: T.Text -- ^Title of the website
  , cfgDesc  :: T.Text -- ^Description of the website
  , cfgDbConnStr :: String -- ^db connection string
  }
  deriving (Show, Eq, Ord)

-- | Which mode to run spock
data Command
  = HTTP Int
  | HTTPS TLSConfig
  | Both Int TLSConfig
  deriving (Show, Read)

-- | Requires the needed values for runTLS
data TLSConfig = TLSConfig
  { tlsPort :: Int
  , tlsCert :: FilePath
  , tlsKey  :: FilePath
  }
  deriving (Show, Read)

-- | Default configuration to run gather
defaultConfig :: AppConfig
defaultConfig = AppConfig
  { cfgTitle = "Gathering"
  , cfgDesc  = "Get together!"
  , cfgDbConnStr = "host=localhost dbname=gather port=5432 user=gather password=gather"
  }

--------------------
-- Options Parser --
--------------------

data Params = Params
  { pFileCfg :: Maybe FilePath
  , pCfg :: Maybe AppConfig
  , pCmd :: Maybe Command
  }
  deriving (Show)

paramsParserInfo :: ParserInfo Params
paramsParserInfo =
  info (helper <*> (Params <$> optional fromFile <*> optional config <*> optional cmd)) $
     fullDesc
  <> header "Gathering - Publish your events"

config :: Parser AppConfig
config = AppConfig
  <$> fmap T.pack ttl
  <*> fmap T.pack desc
  <*> dbconnstr
  where
    ttl =
      strOption
        (long "title"
         <> short 't'
         <> metavar "NAME"
         <> help "Website title"
        )
    desc =
      strOption
        (long "description"
         <> short 'd'
         <> metavar "DESC"
         <> help "Website description"
        )
    dbconnstr =
      strOption
        (long "dbconnection"
         <> short 'c'
         <> metavar "DBCONN"
         <> help "Database connection string"
        )



cmd :: Parser Command
cmd =
  subparser
  ( command "http" (info (HTTP <$> httpConfig <**> helper)
      ( progDesc "Run only in HTTP mode" ))
 <> command "https" (info (HTTPS <$> tlsConfig <**> helper)
      ( progDesc "Run only in TLS mode" ))
 <> command "both" (info (Both <$> httpConfig <*> tlsConfig <**> helper)
      ( progDesc "Run both in HTTP and TLS modes" ))
  )

httpConfig :: Parser Int
httpConfig =
  option auto
  (long "port"
   <> short 'p'
   <> metavar "PORT"
   <> help "Port for HTTP"
   <> showDefault
   <> value 80
  )

tlsConfig :: Parser TLSConfig
tlsConfig = TLSConfig
  <$> option auto (long "tls-port" <> short 'P' <> metavar "PORT" <> help "Port for TLS" <> showDefault <> value 443)
  <*> strOption (long "tls-key"  <> short 'k' <> metavar "KEY"  <> help "Key file for for TLS")
  <*> strOption (long "tls-cert" <> short 'c' <> metavar "CERT" <> help "Cert file for for TLS")

fromFile :: Parser FilePath
fromFile =
  strOption
  (long "config"
   <> short 'f'
   <> metavar "FILE"
   <> help "Path to configuration file"
  )

