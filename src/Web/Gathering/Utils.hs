{- | Utility functions
-}

module Web.Gathering.Utils where

import Turtle (err)
import Data.Time
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

trd4 :: (a, b, c, d) -> c
trd4 (_, _, c, _) = c

frh4 :: (a, b, c, d) -> d
frh4 (_, _, _, d) = d

-- | trim spaces from both sides
trim :: T.Text -> T.Text
trim = T.filter (/='\r') . T.reverse . T.dropWhile (==' ') . T.reverse . T.dropWhile (==' ')


---------------------------
-- Parse and format Time --
---------------------------

parseDateTime :: T.Text -> Maybe UTCTime
parseDateTime = parseTimeM True defaultTimeLocale "%F %H:%M (%Z)" . T.unpack

formatDateTime :: UTCTime -> T.Text
formatDateTime = T.pack . formatTime defaultTimeLocale "%F %H:%M (%Z)"

-- | Format DiffTime as HH:MM
formatDiffTime :: DiffTime -> T.Text
formatDiffTime = T.reverse . T.drop 3 . T.reverse . T.pack . show . timeToTimeOfDay

-- | try to parse the format 'HH:MM' to DiffTime
parseDiffTime :: T.Text -> Maybe DiffTime
parseDiffTime (trim -> T.unpack -> duration) =
  case duration of
    h1:h2:':':m1:m2:[]
      | isDigit h1 && isDigit h2 && read [h1,h2] <= 23
      , isDigit m1 && isDigit m2 && read [m1,m2] <= 59
     -> pure . secondsToDiffTime $ (read [h1,h2] * 60 * 60) + (read [m1,m2] * 60)

    _ -> Nothing
  where
    isDigit = (`elem` ['0'..'9'])

putStrTime :: T.Text -> IO ()
putStrTime txt = do
  t <- getCurrentTime
  T.putStrLn ("[" <> formatDateTime t <> "] - " <> txt)

errTime :: T.Text -> IO ()
errTime txt = do
  t <- getCurrentTime
  err ("<<" <> formatDateTime t <> ">> - " <> txt)
