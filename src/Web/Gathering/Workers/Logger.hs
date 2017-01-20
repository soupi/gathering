{- | A logger proccess that will be comunicated via a queue

Want to print something to the console? report an error or warning? send a message to this fella!

-}

module Web.Gathering.Workers.Logger where

import Web.Gathering.Utils (formatDateTime)

import System.IO
import Data.Monoid
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Control.Monad (void,forever, (<=<))
import Control.Concurrent (forkIO)
import qualified Data.Text.IO as T
import qualified Control.Concurrent.STM as C

-- | Type of message you can send
data Msg
  = Put Text
  | Warn Text
  | Error Text
  deriving (Show, Eq, Ord)

-- | Send a Put message to queue
put :: C.TQueue Msg -> Text -> IO ()
put = act Put

-- | Send a Warn message to queue
warn :: C.TQueue Msg -> Text -> IO ()
warn = act Warn

-- | Send a Error message to queue
err :: C.TQueue Msg -> Text -> IO ()
err = act Error

-- | Send a Msg to queue
act :: (Text -> Msg) -> C.TQueue Msg -> Text -> IO ()
act action q txt =
  C.atomically $
    C.writeTQueue q (action txt)

-- | How to handle messages
type Router = Msg -> IO ()

-- | Create a queue and forever run a logger to handle the messages in the standard way
runDefaultLogger :: IO (C.TQueue Msg)
runDefaultLogger =
  runLogger defaultRouter

-- | Create a queue and forever run a logger to handle the messages with the router supplied
runLogger :: Router -> IO (C.TQueue Msg)
runLogger router = do
  queue <- C.newTQueueIO
  void $ forkIO $ forever $ handler router queue
  pure queue

-- | Run a logger to handle the messages with the router supplied
handler :: Router -> C.TQueue Msg -> IO ()
handler router queue = do
  msg <- C.atomically (C.readTQueue queue)
  router msg

-- | Default router will print to stdout Put and Warn messages
--   and will print to stderr Error messages
--   all with current timestamp
--
defaultRouter :: Router
defaultRouter =
  msgRouters
    (T.putStrLn <=< strTime)
    (T.putStrLn <=< strTime)
    (T.hPutStrLn stdout <=< strTime)

-- | Utility to create a router
msgRouters :: (Text -> IO ()) -> (Text -> IO ()) -> (Text -> IO ()) -> Router
msgRouters put' warn' err' = \case
  Put txt -> put' txt
  Warn tx -> warn' tx
  Error t -> err'  t

-- | Add current time to text message
strTime :: Text -> IO Text
strTime txt = do
  t <- getCurrentTime
  pure ("[" <> formatDateTime t <> "]: " <> txt)
