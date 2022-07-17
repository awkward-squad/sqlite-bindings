module Sqlite3.Bindings.Internal.Utils
  ( boolToCInt,
    cintToInt,
    intToCInt,
    cuintToWord,
    wordToCUInt,
    doubleToCDouble,
    cstringToText,
    cstringLenToText,
    textToCString,
    textToCStringLen,
    carrayToArray,
  )
where

import Control.Monad (when)
import Data.Array (Array)
import qualified Data.Array.IO as Array.IO
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Foreign.C (CChar, CDouble, CInt, CString, CUInt)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable

boolToCInt :: Bool -> CInt
boolToCInt = \case
  False -> 0
  True -> 1

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

carrayToArray :: forall a b. Storable a => (a -> IO b) -> CInt -> Ptr a -> IO (Array Int b)
carrayToArray convert (cintToInt -> n) p0 = do
  arr <- Array.IO.newArray_ @Array.IO.IOArray @b (0, n - 1)
  let loop :: Int -> Ptr a -> IO ()
      loop !i p =
        when (i < n) do
          a <- Storable.peek p
          b <- convert a
          Array.IO.writeArray arr i b
          loop (i + 1) (p0 `plusPtr` Storable.sizeOf (undefined :: Ptr ()))
  loop 0 p0
  Array.IO.freeze arr
