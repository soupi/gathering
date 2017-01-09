module Web.Gathering.HashPassword where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Crypto.PasswordStore as C


makePassword :: String -> IO BS.ByteString
makePassword = flip C.makePassword 15 . BSC.pack

verifyPassword :: BS.ByteString -> BS.ByteString -> Bool
verifyPassword = C.verifyPassword
