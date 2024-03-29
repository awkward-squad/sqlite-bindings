module Sqlite3.Bindings.Internal.Utils
  ( -- * Conversions

    -- ** Bool/CInt
    cintToBool,
    boolToCInt,

    -- ** Int/CInt
    cintToInt,
    intToCInt,

    -- ** Word/CUInt
    cuintToWord,
    wordToCUInt,

    -- ** Double/CDouble
    cdoubleToDouble,
    doubleToCDouble,

    -- ** Text/CString
    cstringToText,
    textToCString,

    -- ** Text/CStringLen
    cstringLenToText,
    textToCStringLen,

    -- ** C array / Array
    carrayToArray,
  )
where

import Control.Monad (when)
import Data.Array (Array)
import qualified Data.Array.IO as Array.IO
import Data.Text (Text)
import qualified Data.Text.Foreign as Text
import Data.Text.Internal (Text (Text))
import Data.Word (Word8)
import Foreign.C (CChar, CDouble, CInt, CString, CUInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable

cintToBool :: CInt -> Bool
cintToBool = \case
  0 -> False
  _ -> True
{-# INLINE cintToBool #-}

boolToCInt :: Bool -> CInt
boolToCInt = \case
  False -> 0
  True -> 1
{-# INLINE boolToCInt #-}

cintToInt :: CInt -> Int
cintToInt =
  fromIntegral
{-# INLINE cintToInt #-}

intToCInt :: Int -> CInt
intToCInt =
  fromIntegral
{-# INLINE intToCInt #-}

cuintToWord :: CUInt -> Word
cuintToWord =
  fromIntegral
{-# INLINE cuintToWord #-}

wordToCUInt :: Word -> CUInt
wordToCUInt =
  fromIntegral
{-# INLINE wordToCUInt #-}

cdoubleToDouble :: CDouble -> Double
cdoubleToDouble =
  realToFrac
{-# INLINE cdoubleToDouble #-}

doubleToCDouble :: Double -> CDouble
doubleToCDouble =
  realToFrac
{-# INLINE doubleToCDouble #-}

cstringToText :: CString -> IO Text
cstringToText string =
  Text.fromPtr0 (castPtr string)
{-# INLINE cstringToText #-}

textToCString :: Text -> (CString -> IO a) -> IO a
textToCString text@(Text _ _ len) action =
  allocaBytes (len + 1) \ptr -> do
    Text.unsafeCopyToPtr text ptr
    Storable.pokeByteOff ptr len (0 :: Word8)
    action (castPtr ptr)
{-# INLINE textToCString #-}

cstringLenToText :: Ptr CChar -> Text.I8 -> IO Text
cstringLenToText c_string len =
  Text.fromPtr (castPtr c_string) len
{-# INLINE cstringLenToText #-}

textToCStringLen :: forall a. Text -> (Ptr CChar -> Int -> IO a) -> IO a
textToCStringLen text@(Text _ _ len) action =
  allocaBytes len \ptr -> do
    Text.unsafeCopyToPtr text ptr
    action (castPtr ptr) len
{-# INLINE textToCStringLen #-}

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
{-# INLINE carrayToArray #-}
