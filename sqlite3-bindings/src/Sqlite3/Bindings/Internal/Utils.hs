module Sqlite3.Bindings.Internal.Utils
  ( cstringToText,
    cstringLenToText,
    textToCString,
    textToCStringLen,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Foreign.C (CChar, CString)
import Foreign.Ptr (Ptr)

cstringToText :: CString -> IO Text
cstringToText string =
  Text.decodeUtf8 <$> ByteString.packCString string

cstringLenToText :: Ptr CChar -> Int -> IO Text
cstringLenToText c_string len =
  Text.decodeUtf8 <$> ByteString.unsafePackCStringLen (c_string, len)

textToCString :: Text -> (CString -> IO a) -> IO a
textToCString =
  ByteString.useAsCString . Text.encodeUtf8

textToCStringLen :: Text -> (Ptr CChar -> Int -> IO a) -> IO a
textToCStringLen text action =
  ByteString.unsafeUseAsCString bytes \c_string ->
    action c_string (ByteString.length bytes)
  where
    bytes = Text.encodeUtf8 text
