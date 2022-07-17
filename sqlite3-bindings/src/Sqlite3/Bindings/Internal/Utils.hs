module Sqlite3.Bindings.Internal.Utils
  ( cintToInt,
    intToCInt,
    cuintToWord,
    wordToCUInt,
    doubleToCDouble,
    cstringToText,
    cstringLenToText,
    textToCString,
    textToCStringLen,
    cstringsToTexts,
  )
where

import Control.Monad (when)
import Data.Array (Array)
import qualified Data.Array.IO as Array.IO
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Foreign.C (CChar, CDouble, CInt, CString, CUInt)
import Foreign.Ptr (Ptr, castPtr, plusPtr)

cintToInt :: CInt -> Int
cintToInt =
  fromIntegral

intToCInt :: Int -> CInt
intToCInt =
  fromIntegral

cuintToWord :: CUInt -> Word
cuintToWord =
  fromIntegral

wordToCUInt :: Word -> CUInt
wordToCUInt =
  fromIntegral

doubleToCDouble :: Double -> CDouble
doubleToCDouble =
  realToFrac

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

cstringsToTexts :: CInt -> Ptr CString -> IO (Array Int (Either Text.UnicodeException Text))
cstringsToTexts (cintToInt -> n) c_string0 = do
  texts <- Array.IO.newArray_ @Array.IO.IOArray @(Either Text.UnicodeException Text) (0, n - 1)
  let loop :: Int -> Ptr CString -> IO ()
      loop !i (castPtr @CString @CChar -> c_string) =
        when (i < n) do
          bytes <- ByteString.packCString c_string
          print (c_string, bytes)
          Array.IO.writeArray texts i (Right "placeholder") -- (Text.decodeUtf8' bytes)
          loop (i + 1) (c_string `plusPtr` ByteString.length bytes)
  loop 0 c_string0
  Array.IO.freeze texts
