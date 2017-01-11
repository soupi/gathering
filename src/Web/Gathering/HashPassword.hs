{- | Hash a password before inserting it to the db
-}

module Web.Gathering.HashPassword where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Crypto.PasswordStore as C

makePassword :: T.Text -> IO BS.ByteString
makePassword = flip C.makePassword 15 . T.encodeUtf8

verifyPassword :: BS.ByteString -> BS.ByteString -> Bool
verifyPassword = C.verifyPassword
