module Sqlite3.Bindings.Internal.Utils
  ( cstringToText,
    textToCString,
  )
where

import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Foreign.C.String (CString)

cstringToText :: CString -> IO Text
cstringToText string =
  Text.decodeUtf8 <$> ByteString.packCString string

textToCString :: Text -> (CString -> IO a) -> IO a
textToCString =
  ByteString.useAsCString . Text.encodeUtf8
