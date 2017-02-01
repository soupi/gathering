{- | Hash a password before inserting it to the db
-}

module Web.Gathering.HashPassword where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Crypto.PasswordStore as C
import Data.Monoid ((<>))
import Data.Digest.Pure.MD5

makePassword :: T.Text -> IO BS.ByteString
makePassword = flip C.makePassword 15 . T.encodeUtf8

verifyPassword :: BS.ByteString -> BS.ByteString -> Bool
verifyPassword = C.verifyPassword

hashMD5 :: T.Text -> T.Text -> T.Text
hashMD5 k t = T.pack . show . md5 $ (TL.encodeUtf8 $ TL.fromStrict k) <> (TL.encodeUtf8 $ TL.fromStrict t)
